module Orn.Registry.OpenApiProcessing

open Orn.Registry.JsonLdParsing
open Orn.Registry.BasicTypes
open DouglasConnect.Http
open Cvdm.ErrorHandling
open Orn.Registry.OpenApiTransformer
open System.Threading
open Orn.Registry.Shared

type Message =
  | AddToIndex of OpenApiUrl
  | RemoveFromIndex of OpenApiUrl
  | ReindexFailed

type OpenRiskNetServiceInfo =
  { TripleStore : VDS.RDF.TripleStore
    OpenApiServiceInformation : OpenApiServiceInformation
  }

type TripleIndexingStatus =
  | InProgress
  | Indexed of OpenRiskNetServiceInfo
  | Failed of string

type OpenApiProcessingInformation =
  { Status : TripleIndexingStatus
    RawOpenApi : OpenApiRaw option
    DereferencedOpenApi : OpenApiFixedContextEntry option
    }

type IOpenApiProcessingAgent =
  Orn.Registry.IAgent<Map<OpenApiUrl,OpenApiProcessingInformation>, Message>

let updateMap key newval map =
  let mutable found = false
  let newMap =
    map
    |> Map.map (fun k v ->
      if k = key then
        found <- true
        newval
      else
        v)
  if found then
    Some newMap
  else
    None


type OpenApiAgent(feedbackAgent : IAgent<seq<TimestampedFeedback>, Feedback>, cancelToken : CancellationToken) =
  let mutable serviceMap : Map<OpenApiUrl,OpenApiProcessingInformation> = Map []

  let updateServiceMap key newval =
    let updatedMapOption = updateMap key newval serviceMap
    match updatedMapOption with
    | Some updatedMap ->
      serviceMap <- updatedMap
    | None ->
      printfn "Could not find key %O" key
      ()



  let rec agentFunction (feedbackAgent : Feedback.IFeedbackAgent) (agent : Agent<Message>) =
    async {

      let! message = agent.Receive()
      let (>>=) a b = Result.bind b a
      let makeTuple a b = (a,b)
      match message with
      | AddToIndex ((OpenApiUrl url) as openApiUrl) ->
          serviceMap <- serviceMap |> Map.add openApiUrl { Status = InProgress; RawOpenApi = None; DereferencedOpenApi = None }
          try
            let! result =
              asyncResult {
                printfn "Downloading openapi definition for %s" url
                let headers = [ FSharp.Data.HttpRequestHeaders.Accept FSharp.Data.HttpContentTypes.Json ]
                let! openapistring =
                  SafeAsyncHttp.AsyncHttpTextResult(url, timeout=System.TimeSpan.FromSeconds(20.0), headers=headers)
                  |> AsyncResult.mapError (fun err -> err.ToString())
                  |> AsyncResult.teeError (fun _ -> feedbackAgent.Post(Orn.Registry.Shared.OpenApiDownloadFailed(openApiUrl)))
                updateServiceMap openApiUrl { Status = InProgress; RawOpenApi = Some (OpenApiRaw openapistring ); DereferencedOpenApi = None}
                printfn "Downloading worked, processing..."

                let retrievedAt = System.DateTime.UtcNow

                let! description, openapi =
                  openapistring
                  |> OpenApiRaw
                  |> TransformOpenApiToV3Dereferenced retrievedAt (openApiUrl)
                  |> Result.teeError (fun err -> feedbackAgent.Post(Orn.Registry.Shared.OpenApiParsingFailed(openApiUrl, err)))
                let! jsonld =
                  fixOrnJsonLdContext openapi
                  |> Result.teeError (fun err -> feedbackAgent.Post(Orn.Registry.Shared.JsonLdParsingError(openApiUrl, err)))
                updateServiceMap openApiUrl {Status = InProgress; RawOpenApi = Some (OpenApiRaw openapistring); DereferencedOpenApi = Some (OpenApiFixedContextEntry (jsonld.Unwrap()))}
                let! tripleStore =
                  loadJsonLdIntoTripleStore jsonld
                  |> Result.teeError (fun err -> feedbackAgent.Post(Orn.Registry.Shared.JsonLdParsingError(openApiUrl, err)))
                return (description, tripleStore)
              }

            let processingInfo = serviceMap |> Map.find openApiUrl

            match result with
            | Ok (serviceInformation, tripleStore) ->
                printfn "Loading json-ld into triple store worked for service %s" url
                updateServiceMap openApiUrl { processingInfo with Status = Indexed {TripleStore = tripleStore; OpenApiServiceInformation = serviceInformation} }
            | Error msg ->
                printfn "Loading json-ld into triple store FAILED for service %s" url
                updateServiceMap openApiUrl { processingInfo with Status = Failed msg }

          with
          | :? System.Net.WebException ->
            printfn "Timeout occured in OpenApi processing agent when processing: %s" url
            let updatedMap = (serviceMap |> updateMap (OpenApiUrl url) ({ Status = Failed "Timeout while trying to download swagger definition"; RawOpenApi = None; DereferencedOpenApi = None}))
            match updatedMap with
            | Some updated ->
                serviceMap <- updated
            | _ -> ()
          | ex ->
            feedbackAgent.Post(Orn.Registry.Shared.JsonLdParsingError(OpenApiUrl url, ex.ToString()))
            printfn "Exception occured in OpenApi processing agent: %O" ex

      | RemoveFromIndex url ->
          serviceMap <- serviceMap |> Map.remove url

      | ReindexFailed ->
          serviceMap <-
            Map.fold (fun state key value ->
            match value.Status with
             | InProgress -> Map.add key value state
             | Indexed _ -> Map.add key value state
             | Failed _ ->
                agent.Post(AddToIndex(key))
                state // drop failed services after queing them for reindexing
            ) Map.empty serviceMap

      do! agentFunction feedbackAgent agent
    }

  let agent = Agent.Start(agentFunction feedbackAgent, cancelToken)

  interface IOpenApiProcessingAgent with
    member this.Post(message : Message) = agent.Post(message)
    member this.ReadonlyState = serviceMap //TODO: if there is a weird error that no services sho up then the reason could be that this is a closure which I hope it is not
