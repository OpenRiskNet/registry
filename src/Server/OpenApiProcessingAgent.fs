module Orn.Registry.OpenApiProcessing

open Orn.Registry.JsonLdParsing
open Orn.Registry.BasicTypes
open DouglasConnect.Http
open Cvdm.ErrorHandling
open Orn.Registry.OpenApiTransformer
open System.Threading
open Orn.Registry.Shared
open Orn.Registry.OpenApiServicesAgent

type ProcessingMessage =
  | IndexNewUrl of OpenApiUrl
  | RemoveUrl of OpenApiUrl

type IOpenApiProcessingAgent = Orn.Registry.IAgent<bool, ProcessingMessage> // The state should be unit but there is a weird compiler error if I return unit

type OpenApiProcessingAgent(feedbackAgent : Feedback.IFeedbackAgent, servicesAgent : IOpenApiServicesAgent, cancelToken : CancellationToken) =
  let rec agentFunction (feedbackAgent : Feedback.IFeedbackAgent) (servicesAgent : IOpenApiServicesAgent) (agent : Agent<ProcessingMessage>) =
    async {

      let! message = agent.Receive()
      let (>>=) a b = Result.bind b a
      let makeTuple a b = (a,b)
      match message with
      | IndexNewUrl ((OpenApiUrl url) as openApiUrl) ->
          let mutable (processingInfo : OpenApiProcessingInformation) = { Status = InProgress; RawOpenApi = None; DereferencedOpenApi = None }

          let updateProcessingInfo (info : OpenApiProcessingInformation) =
            processingInfo <- info
            servicesAgent.Post(AddService (openApiUrl, info))

          updateProcessingInfo { Status = InProgress; RawOpenApi = None; DereferencedOpenApi = None }

          try
            let! result =
              asyncResult {
                printfn "Downloading openapi definition for %s" url
                let headers = [ FSharp.Data.HttpRequestHeaders.Accept FSharp.Data.HttpContentTypes.Json ]
                let! openapistring =
                  SafeAsyncHttp.AsyncHttpTextResult(url, timeout=System.TimeSpan.FromSeconds(20.0), headers=headers)
                  |> AsyncResult.mapError (fun err -> err.ToString())
                  |> AsyncResult.teeError (fun _ -> feedbackAgent.Post(Orn.Registry.Shared.OpenApiDownloadFailed(openApiUrl)))
                updateProcessingInfo { Status = InProgress; RawOpenApi = Some (OpenApiRaw openapistring ); DereferencedOpenApi = None}
                printfn "Downloading worked, processing..."

                let retrievedAt = System.DateTime.UtcNow

                let! description, openapi =
                  openapistring
                  |> OpenApiRaw
                  |> TransformOpenApiToV3Dereferenced retrievedAt (openApiUrl)
                  |> Result.teeError (fun err -> feedbackAgent.Post(OpenApiParsingFailed(openApiUrl, err)))
                let! jsonld =
                  fixOrnJsonLdContext openapi
                  |> Result.teeError (fun err -> feedbackAgent.Post(JsonLdParsingError(openApiUrl, err)))
                updateProcessingInfo {Status = InProgress; RawOpenApi = Some (OpenApiRaw openapistring); DereferencedOpenApi = Some (OpenApiFixedContextEntry (jsonld.Unwrap()))}
                let! tripleStore =
                  loadJsonLdIntoTripleStore jsonld
                  |> Result.teeError (fun err -> feedbackAgent.Post(JsonLdParsingError(openApiUrl, err)))
                return (description, tripleStore)
              }



            match result with
            | Ok (serviceInformation, tripleStore) ->
                printfn "Loading json-ld into triple store worked for service %s" url
                servicesAgent.Post(AddService (openApiUrl, { processingInfo with Status = Indexed {TripleStore = tripleStore; OpenApiServiceInformation = serviceInformation} }))
            | Error msg ->
                printfn "Loading json-ld into triple store FAILED for service %s" url
                servicesAgent.Post(AddService (openApiUrl, { processingInfo with Status = Failed msg }))

          with
          | :? System.Net.WebException ->
            printfn "Timeout occured in OpenApi processing agent when processing: %s" url
            servicesAgent.Post(AddService (openApiUrl,({ Status = Failed "Timeout while trying to download swagger definition"; RawOpenApi = None; DereferencedOpenApi = None})))

          | ex ->
            feedbackAgent.Post(JsonLdParsingError(OpenApiUrl url, ex.ToString()))
            printfn "Exception occured in OpenApi processing agent: %O" ex

      | RemoveUrl url ->
        servicesAgent.Post(RemoveService url)

      // | ReindexFailed ->
      //     serviceMap <-
      //       Map.fold (fun state key value ->
      //       match value.Status with
      //        | InProgress -> Map.add key value state
      //        | Indexed _ -> Map.add key value state
      //        | Failed _ ->
      //           agent.Post(AddToIndex(key))
      //           state // drop failed services after queing them for reindexing
      //       ) Map.empty serviceMap

      do! agentFunction feedbackAgent servicesAgent agent
    }

  let agent = Agent.Start(agentFunction feedbackAgent servicesAgent, cancelToken)

  interface IOpenApiProcessingAgent with
    member this.Post(message : ProcessingMessage) = agent.Post(message)
    member this.ReadonlyState = true //TODO: if there is a weird error that no services sho up then the reason could be that this is a closure which I hope it is not
