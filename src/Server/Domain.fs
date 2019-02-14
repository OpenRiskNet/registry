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
open DouglasConnect.Http

let mutable ExternalServices = Set.empty<string>

let getCurrentServices (logger : ILogger) : Async<Shared.ActiveServices> =
  async {

    logger.LogInformation "Entered getCurrentServices"

    let k8sServices =
      k8sUpdateAgent.Services

    let ornServices =
      openApiAgent.ServiceMap

    let externalServices =
      ExternalServices

    let externalServiceWithOptionalOrnService =
      ExternalServices
      |> Seq.choose  ((fun openApiUrl -> ornServices |> Map.tryFind (Shared.OpenApiUrl openApiUrl) )
                     >> (fun indexStatusOption ->
                          match indexStatusOption with
                          | None -> None
                          | Some OpenApiProcessing.InProgress -> None
                          | Some (OpenApiProcessing.Failed _) -> None
                          | Some (OpenApiProcessing.Indexed ornInfo) -> Some { OpenApiServiceInformation = ornInfo.OpenApiServiceInformation } ))
      |> Seq.toList

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
        Shared.ExternalOrnServices = externalServiceWithOptionalOrnService
        Shared.ExternalServices = Set.toList externalServices
        Shared.Messages = feedbackAgent.Log |> Seq.toList }

  }


let getCurrentServicesHandler : HttpHandler =
    fun next (ctx : Http.HttpContext) ->
      task {
        let logger = ctx.GetLogger()
        let! services = getCurrentServices(logger)
        return! Giraffe.HttpStatusCodeHandlers.Successful.ok (json services) next ctx
      }


let addExternalServiceHandler : HttpHandler =
  fun next (ctx : Http.HttpContext) ->
    task {
      let logger = ctx.GetLogger()
      logger.LogInformation("Adding external service")
      let hasService, service = ctx.Request.Query.TryGetValue "service"
      if not hasService then
        return! Giraffe.HttpStatusCodeHandlers.RequestErrors.BAD_REQUEST (text "Could not find query parameter 'service'") next ctx
      else
        logger.LogInformation("Adding service: ", service)
        do openApiAgent.SendMessage(Orn.Registry.OpenApiProcessing.AddToIndex (OpenApiUrl (service.[0])))
        ExternalServices <- Set.add service.[0] ExternalServices
        return! Giraffe.HttpStatusCodeHandlers.Successful.NO_CONTENT next ctx
    }


let removeExternalServiceHandler : HttpHandler =
  fun next (ctx : Http.HttpContext) ->
    task {
      let logger = ctx.GetLogger()
      logger.LogInformation("Adding external service")
      let hasService, service = ctx.Request.Query.TryGetValue "service"
      if not hasService then
        return! Giraffe.HttpStatusCodeHandlers.RequestErrors.BAD_REQUEST (text "Could not find query parameter 'service'") next ctx
      else
        logger.LogInformation("Removing service: ", service)
        do openApiAgent.SendMessage(Orn.Registry.OpenApiProcessing.RemoveFromIndex (OpenApiUrl (service.[0])))
        ExternalServices <- Set.remove service.[0] ExternalServices
        return! Giraffe.HttpStatusCodeHandlers.Successful.NO_CONTENT next ctx
    }


let runSparqlQuery (logger : ILogger) (maybeService : OpenApiUrl option) (query : string) : Result<SparqlResultsForServices, string> =
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
    let sources =
      match maybeService with
      | None -> openApisAndTripleStores
      | Some service -> openApisAndTripleStores |> List.filter (fun (openapiUrl, _, _) -> openapiUrl = service)

    let results =
      sources
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
      let hasService, serviceValues = ctx.Request.Query.TryGetValue "service"
      if not hasQuery then
        return! Giraffe.HttpStatusCodeHandlers.RequestErrors.BAD_REQUEST (text "Could not find query parameter 'query'") next ctx
      else
        logger.LogInformation("Query is:", query)
        let service =
          if hasService
          then Some (OpenApiUrl serviceValues.[0])
          else None
        let result = runSparqlQuery logger service (query.[0])
        match result with
        | Ok resultTriplesForServices -> // TODO: serialize this properly
            return! Giraffe.HttpStatusCodeHandlers.Successful.ok (json resultTriplesForServices) next ctx
        | Error error ->
            return! Giraffe.HttpStatusCodeHandlers.ServerErrors.INTERNAL_ERROR (text error) next ctx
    }


let swaggerUiHandler : HttpHandler =
    fun next (ctx : Http.HttpContext) ->
        task {
          let hasService, serviceUris = ctx.Request.Query.TryGetValue "service"
          if hasService then
            let serviceUri = serviceUris.[0]
            let services = openApiAgent.ServiceMap
            let registeredService =
              Map.tryFind (OpenApiUrl serviceUri) services
              |> Option.map (fun serviceIndexingStatus ->
                  match serviceIndexingStatus with
                   | InProgress -> None
                   | Indexed serviceInfo -> Some serviceInfo
                   | Failed _ -> None)
            let headers = [ FSharp.Data.HttpRequestHeaders.Accept FSharp.Data.HttpContentTypes.Json ]
            let! openapiContentResult = Async.StartAsTask (SafeAsyncHttp.AsyncHttpTextResult(serviceUri, timeout=System.TimeSpan.FromSeconds(20.0), headers=headers))
            match openapiContentResult with
            | Ok openapiContent -> return! htmlView (Orn.Registry.Views.SwaggerUi.swaggerUi openapiContent) next ctx
            | Error err -> return! Giraffe.HttpStatusCodeHandlers.ServerErrors.INTERNAL_ERROR "Could not retrieve Openapi document" next ctx
          else
            return! Giraffe.HttpStatusCodeHandlers.RequestErrors.NOT_FOUND "Please specify a valid service url in the service query parameter!" next ctx
        }
