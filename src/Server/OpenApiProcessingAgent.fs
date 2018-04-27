module Orn.Registry.OpenApiProcessing

open Orn.Registry.JsonLdParsing
open Orn.Registry.BasicTypes
open DouglasConnect.Http
open Cvdm.ErrorHandling
open Orn.Registry.OpenApiTransformer
open System.Threading

type Message =
  | AddToIndex of SwaggerUrl
  | RemoveFromIndex of SwaggerUrl

type TripleIndexingStatus =
  | InProgress
  | Indexed of VDS.RDF.TripleStore
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
      match message with
      | AddToIndex (SwaggerUrl url) ->
          serviceMap <- serviceMap |> Map.add (SwaggerUrl url) InProgress
          let! result =
            asyncResult {
              let! openapistring = SafeAsyncHttp.AsyncHttpTextResult url |> AsyncResult.mapError (fun err -> err.ToString())
              return!
                openapistring
                |> OpenApiRaw
                |> TransformOpenApiToV3Dereferenced
                >>= fixOrnJsonLdContext
                >>= loadJsonLdIntoTripleStore
            }

          match result with
          | Ok tripleStore ->
              let updateMap = serviceMap |> updateMap (SwaggerUrl url) (Indexed tripleStore)
              match updateMap with
              | Some map ->
                  serviceMap <- map
              | None ->
                  printfn "Update of map failed: %s" url
          | Error msg ->
              let updateMap = serviceMap |> updateMap (SwaggerUrl url) (Failed msg)
              match updateMap with
              | Some map ->
                  serviceMap <- map
              | None ->
                  printfn "Update of map failed: %s" url
      | RemoveFromIndex url ->
          serviceMap <- serviceMap |> Map.remove url
      do! agentFunction(agent)
    }

  let agent = Agent.Start(agentFunction, cancelToken)
