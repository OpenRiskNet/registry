module Orn.Registry.OpenApiProcessing

open Orn.Registry.JsonLdParsing
open Orn.Registry.BasicTypes
open DouglasConnect.Http
open Cvdm.ErrorHandling
open Orn.Registry.OpenApiTransformer
open System.Threading
open Orn.Registry.Shared

type Message =
  | AddToIndex of SwaggerUrl
  | RemoveFromIndex of SwaggerUrl

type OpenRiskNetServiceInfo =
  { TripleStore : VDS.RDF.TripleStore
    OpenApiServiceInformation : OpenApiServiceInformation
  }

type TripleIndexingStatus =
  | InProgress
  | Indexed of OpenRiskNetServiceInfo
  | Failed of string

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


type OpenApiAgent(cancelToken : CancellationToken) =
  let mutable serviceMap : Map<SwaggerUrl,TripleIndexingStatus> = Map []

  let rec agentFunction (agent : Agent<Message>) =
    async {
      let! message = agent.Receive()
      let (>>=) a b = Result.bind b a
      let makeTuple a b = (a,b)
      match message with
      | AddToIndex (SwaggerUrl url) ->
          serviceMap <- serviceMap |> Map.add (SwaggerUrl url) InProgress
          let! result =
            asyncResult {
              printfn "Downloading openapi definition for %s" url
              let! openapistring = SafeAsyncHttp.AsyncHttpTextResult(url, timeout=2000) |> AsyncResult.mapError (fun err -> err.ToString())
              printfn "Downloading worked, processing..."
              return!
                openapistring
                |> OpenApiRaw
                |> TransformOpenApiToV3Dereferenced (SwaggerUrl url)
                >>= (fun (description, openapi) -> fixOrnJsonLdContext openapi |?> makeTuple description )
                >>= (fun (description, jsonld) -> loadJsonLdIntoTripleStore jsonld |?> makeTuple description)
            }

          let updatedMap =
            match result with
            | Ok (serviceInformation, tripleStore) ->
                printfn "Loading json-ld into triple store worked for service %s" url
                serviceMap |> updateMap (SwaggerUrl url) (Indexed {TripleStore = tripleStore; OpenApiServiceInformation = serviceInformation})
            | Error msg ->
                serviceMap |> updateMap (SwaggerUrl url) (Failed msg)

          match updatedMap with
          | Some map ->
              serviceMap <- map
          | None ->
              printfn "Update of map failed: %s" url

      | RemoveFromIndex url ->
          serviceMap <- serviceMap |> Map.remove url

      do! agentFunction(agent)
    }

  let agent = Agent.Start(agentFunction, cancelToken)

  member this.ServiceMap = serviceMap

  member this.SendMessage (msg : Message) = agent.Post msg
