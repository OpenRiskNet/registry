module Orn.Registry.Domain

open Orn.Registry.BasicTypes
open Microsoft.Extensions.Logging

open Giraffe
open Orn.Registry
open Orn.Registry.AgentSetup
open Microsoft.AspNetCore
open FSharp.Control.Tasks.V2.ContextInsensitive
open Orn.Registry.OpenApiProcessing
open Orn.Registry.JsonLdParsing
open Orn.Registry.Shared

let getCurrentServices (logger : ILogger) : Async<Shared.ActiveServices> =
  async {

    logger.LogInformation "Entered getCurrentServices"

    let k8sServices =
      k8sUpdateAgent.Services

    let ornServices =
      openApiAgent.ServiceMap

    let k8sServiceWithOptionalOrnService =
      k8sServices
      |> Seq.map ((fun service -> service.Annotations |> Map.tryFind Shared.Constants.OpenApiLabelStaticServices )
               >> (fun openApiUrlOption -> openApiUrlOption |> Option.bind (fun openApiUrl -> ornServices |> Map.tryFind (Shared.OpenApiUrl openApiUrl) ) )
               >> (fun indexStatusOption ->
                    match indexStatusOption with
                    | None -> None
                    | Some OpenApiProcessing.InProgress -> None
                    | Some (OpenApiProcessing.Failed _) -> None
                    | Some (OpenApiProcessing.Indexed ornInfo) -> Some ornInfo))
      |> Seq.zip (k8sServices)

    let k8sServicesOnly, ornServices =
      k8sServiceWithOptionalOrnService
      |> Seq.fold
          (fun (servicesOnly, ornservices) currentServicePair ->
             match currentServicePair with
             | (service, Some ornService) -> (servicesOnly, ( service, ornService) :: ornservices)
             | (service, None) -> (service :: servicesOnly, ornservices)
          ) ([], [])

    let internalK8sServiceToClientFormat (k8sService : Kubernetes.K8sService) =
      { Shared.Name = k8sService.Name
        Shared.ServicePorts = k8sService.Ports}

    let internalK8sServicesToClientFormat (internalK8sServices : Kubernetes.K8sService list) =
      internalK8sServices
      |> Seq.map internalK8sServiceToClientFormat

    let clientFormatK8sOnlyServices =
      internalK8sServicesToClientFormat k8sServicesOnly
      |> List.ofSeq

    let clientFormatOrnServices =
      ornServices
      |> Seq.map (fun (k8sService, openApiService) -> { Shared.K8sService = internalK8sServiceToClientFormat k8sService
                                                        Shared.OpenApiServiceInformation = openApiService.OpenApiServiceInformation })
      |> List.ofSeq

    return
      { Shared.PlainK8sServices = clientFormatK8sOnlyServices
        Shared.OrnServices = clientFormatOrnServices
        Shared.Messages = feedbackAgent.Log |> Seq.toList }

  }


let getCurrentServicesHandler : HttpHandler =
    fun next (ctx : Http.HttpContext) ->
      task {
        let logger = ctx.GetLogger()
        let! services = getCurrentServices(logger)
        return! Giraffe.HttpStatusCodeHandlers.Successful.ok (json services) next ctx
      }


let runSparqlQuery (logger : ILogger) (query : string) : Result<SparqlResultsForServices, string> =
  result {
    let! parsedQuery = createSparqlQuery query

    let ornServices =
        openApiAgent.ServiceMap

    let openApisAndTripleStores =
      ornServices
      |> Map.fold (fun state key value ->
                    match value with
                    | InProgress -> state
                    | Failed _ -> state
                    | Indexed serviceInfo -> (key, serviceInfo.OpenApiServiceInformation.Name, serviceInfo.TripleStore) :: state ) []

    // log info about operations

    // TODO: we query the triple stores serially for now. If the memory use is ok we could run all
    // these queries in parallel or use some kind of max-parallelism
    let results =
      openApisAndTripleStores
      |> List.map (fun (openapiUrl, name, triples) -> (openapiUrl, name, runQuery triples parsedQuery))

    let resultsWithEmptyListForErrors =
      results
      |> List.map (fun (openapiUrl, name, result) ->
        match result with
        | Ok resultValue ->
            { ServiceName = name
              OpenApiUrl = openapiUrl
              Result = resultValue
            }
        | Error error ->
          logger.LogError (sprintf "Error when running query %s against url %O: %s" query openapiUrl error)
          {   ServiceName = name
              OpenApiUrl = openapiUrl
              Result = NoResult
          }
          )

    return resultsWithEmptyListForErrors
  }

let runSparqlQueryHandler : HttpHandler =
  fun next (ctx : Http.HttpContext) ->
    task {
      let logger = ctx.GetLogger()
      logger.LogInformation("Processing SparQL query")
      let hasQuery, query = ctx.Request.Query.TryGetValue "query"
      if not hasQuery then
        return! Giraffe.HttpStatusCodeHandlers.RequestErrors.BAD_REQUEST (text "Could not find query parameter 'query'") next ctx
      else
        logger.LogInformation("Query is:", query)
        let result = runSparqlQuery logger (query.[0])
        match result with
        | Ok resultTriplesForServices -> // TODO: serialize this properly
            return! Giraffe.HttpStatusCodeHandlers.Successful.ok (json resultTriplesForServices) next ctx
        | Error error ->
            return! Giraffe.HttpStatusCodeHandlers.ServerErrors.INTERNAL_ERROR (text error) next ctx
    }
