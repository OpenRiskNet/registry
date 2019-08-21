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
open System.Security

let mutable ExternalServices = Set.empty<string>

type Email =
  | Email of string

let userEmail (ctx: Http.HttpContext) : Email option =
    if not (isNull ctx.User) && ctx.User.Identity.IsAuthenticated then
        ctx.User.FindFirst Claims.ClaimTypes.Email
        |> function
           | null -> None
           | claim -> claim.Value |> Email |> Some
    else
        None

let requireUserHandler (handler : Email -> HttpHandler) : HttpHandler =
  fun next ctx ->
      match userEmail ctx with
      | Some user -> handler user next ctx
      | None -> challenge Microsoft.AspNetCore.Authentication.JwtBearer.JwtBearerDefaults.AuthenticationScheme next ctx

let getCurrentServices (logger : ILogger) email : Async<Shared.ActiveServices> =
  async {

    logger.LogInformation "Entered getCurrentServices"

    let k8sServices =
      k8sUpdateAgent.ReadonlyState

    let ornServices =
      openApiServicesAgent.ReadonlyState

    let externalServices =
      ExternalServices

    let externalServiceWithOptionalOrnService =
      ExternalServices
      |> Seq.choose  ((fun openApiUrl -> ornServices |> Map.tryFind (Shared.OpenApiUrl openApiUrl) )
                     >> (fun indexStatusOption ->
                          match indexStatusOption with
                          | None -> None
                          | Some { Status = OpenApiServicesAgent.InProgress } -> None
                          | Some { Status = (OpenApiServicesAgent.Failed _) } -> None
                          | Some { Status = (OpenApiServicesAgent.Reindexing ornInfo)}
                          | Some { Status = (OpenApiServicesAgent.Indexed ornInfo) } -> Some { OpenApiServiceInformation = ornInfo.OpenApiServiceInformation } ))
      |> Seq.toList

    let k8sServiceWithOptionalOrnService =
      k8sServices
      |> Seq.map ((fun service -> service.Annotations |> Map.tryFind Shared.Constants.OpenApiLabelStaticServices )
               >> (fun openApiUrlOption -> openApiUrlOption |> Option.bind (fun openApiUrl -> ornServices |> Map.tryFind (Shared.OpenApiUrl openApiUrl) ) )
               >> (fun indexStatusOption ->
                    match indexStatusOption with
                    | None -> None
                    | Some { Status = OpenApiServicesAgent.InProgress } -> None
                    | Some { Status = (OpenApiServicesAgent.Failed _) } -> None
                    | Some { Status = (OpenApiServicesAgent.Reindexing ornInfo) }
                    | Some { Status = (OpenApiServicesAgent.Indexed ornInfo) } -> Some ornInfo))
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
        Shared.Messages = feedbackAgent.ReadonlyState |> Seq.toList }

  }


let getCurrentServicesHandler email : HttpHandler =
    fun next (ctx : Http.HttpContext) ->
      task {
        let logger = ctx.GetLogger()
        let! services = getCurrentServices logger email
        return! Successful.ok (json services) next ctx
      }


let addExternalServiceHandler : HttpHandler =
  fun next (ctx : Http.HttpContext) ->
    task {
      let logger = ctx.GetLogger()
      logger.LogInformation("Adding external service")
      let hasService, service = ctx.Request.Query.TryGetValue "service"
      if not hasService then
        return! RequestErrors.BAD_REQUEST (text "Could not find query parameter 'service'") next ctx
      else
        logger.LogInformation("Adding service: ", service)
        do openApiProcessingAgent.Post(IndexNewUrl (OpenApiUrl (service.[0]), None, 60.0<FSharp.Data.UnitSystems.SI.UnitNames.second>))
        ExternalServices <- Set.add service.[0] ExternalServices
        return! Successful.NO_CONTENT next ctx
    }


let removeExternalServiceHandler : HttpHandler =
  fun next (ctx : Http.HttpContext) ->
    task {
      let logger = ctx.GetLogger()
      logger.LogInformation("Adding external service")
      let hasService, service = ctx.Request.Query.TryGetValue "service"
      if not hasService then
        return! RequestErrors.BAD_REQUEST (text "Could not find query parameter 'service'") next ctx
      else
        logger.LogInformation("Removing service: ", service)
        do openApiProcessingAgent.Post(RemoveUrl (OpenApiUrl (service.[0])))
        ExternalServices <- Set.remove service.[0] ExternalServices
        return! Successful.NO_CONTENT next ctx
    }


let runSparqlQuery (logger : ILogger) (maybeService : OpenApiUrl option) (query : string) : Result<SparqlResultsForServices, string> =
  result {
    let timer = System.Diagnostics.Stopwatch()
    timer.Start()
    let! parsedQuery = createSparqlQuery query
    let timeToParsed = timer.Elapsed
    logger.LogInformation(sprintf "Parsing sparql query took %f milliseconds" timeToParsed.TotalMilliseconds)

    let ornServices =
        openApiServicesAgent.ReadonlyState

    let openApisAndTripleStores =
      ornServices
      |> Map.fold (fun state key value ->
                    match value.Status with
                    | OpenApiServicesAgent.InProgress -> state
                    | OpenApiServicesAgent.Failed _ -> state
                    | OpenApiServicesAgent.Reindexing serviceInfo
                    | OpenApiServicesAgent.Indexed serviceInfo -> (key, serviceInfo.OpenApiServiceInformation.Name, serviceInfo.TripleStore) :: state ) []

    let sources =
      match maybeService with
      | None -> openApisAndTripleStores
      | Some service -> openApisAndTripleStores |> List.filter (fun (openapiUrl, _, _) -> openapiUrl = service)

    let results =
      sources
      |> List.toArray
      |> Array.Parallel.map
        (fun (openapiUrl, name, triples) ->
          let result = runQuery triples parsedQuery
          openapiUrl, name, result
        )

    let resultsWithEmptyListForErrors =
      results
      |> Array.map (fun (openapiUrl, name, result) ->
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

    let timeToQueried = timer.Elapsed
    logger.LogInformation(sprintf "Executing sparql query against %d services took %f milliseconds" (sources.Length) (timeToQueried - timeToParsed).TotalMilliseconds)
    timer.Stop()

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
            return! Successful.ok (json resultTriplesForServices) next ctx
        | Error error ->
            return! ServerErrors.INTERNAL_ERROR (text error) next ctx
    }


let swaggerUiHandler : HttpHandler =
    fun next (ctx : Http.HttpContext) ->
        task {
          let hasService, serviceUris = ctx.Request.Query.TryGetValue "service"
          if hasService then
            let serviceUri = serviceUris.[0]
            let services = openApiServicesAgent.ReadonlyState
            let registeredService =
              Map.tryFind (OpenApiUrl serviceUri) services
              |> Option.bind (fun serviceIndexingStatus ->
                  match serviceIndexingStatus.Status with
                   | OpenApiServicesAgent.InProgress -> None
                   | OpenApiServicesAgent.Indexed serviceInfo -> Some serviceIndexingStatus
                   | OpenApiServicesAgent.Reindexing serviceInfo -> Some serviceIndexingStatus
                   | OpenApiServicesAgent.Failed _ -> None)
            match registeredService with
            | Some { OpenApiRetrievalInformation = Some {OpenApiString = OpenApiRaw rawOpenApi} } ->
              return! htmlView (Orn.Registry.Views.SwaggerUi.swaggerUi rawOpenApi) next ctx
            | _ ->
              return! Giraffe.HttpStatusCodeHandlers.ServerErrors.INTERNAL_ERROR "Could not retrieve OpenApi for requested service" next ctx
          else
            return! Giraffe.HttpStatusCodeHandlers.RequestErrors.NOT_FOUND "Please specify a valid service url in the service query parameter!" next ctx
        }
let prettifyJson (jsonString : string) : Result<string, string> =
  try
    use reader = new System.IO.StringReader(jsonString)
    use writer = new System.IO.StringWriter()
    let jsonReader = new Newtonsoft.Json.JsonTextReader(reader)
    let jsonWriter = new Newtonsoft.Json.JsonTextWriter(writer)
    jsonWriter.Formatting <- Newtonsoft.Json.Formatting.Indented
    jsonWriter.WriteToken(jsonReader)
    writer.ToString() |> Ok
  with
  | ex ->
    Error (ex.Message)
let rawOpenApiHandler : HttpHandler =
  fun next (ctx : Http.HttpContext) ->
    task {
      let hasService, serviceUris = ctx.Request.Query.TryGetValue "service"
      if hasService then
        let serviceUri = serviceUris.[0]
        let services = openApiServicesAgent.ReadonlyState
        let registeredService =
          Map.tryFind (OpenApiUrl serviceUri) services
          |> Option.bind (fun serviceIndexingStatus ->
              match serviceIndexingStatus.Status with
               | OpenApiServicesAgent.InProgress -> None
               | OpenApiServicesAgent.Indexed serviceInfo -> Some serviceIndexingStatus
               | OpenApiServicesAgent.Reindexing serviceInfo -> Some serviceIndexingStatus
               | OpenApiServicesAgent.Failed _ -> None)
        match registeredService with
        | Some { OpenApiRetrievalInformation = Some {OpenApiString = OpenApiRaw rawOpenApi} } ->
            let prettyJsonResult = prettifyJson rawOpenApi
            match prettyJsonResult with
            | Ok prettyJson ->
              ctx.SetHttpHeader "Content-Type" "application/json"
              return! Giraffe.HttpStatusCodeHandlers.Successful.ok (text prettyJson) next ctx
            | Error err ->
              return! Giraffe.HttpStatusCodeHandlers.Successful.ok (text (rawOpenApi)) next ctx
        | _ ->
          return! Giraffe.HttpStatusCodeHandlers.ServerErrors.INTERNAL_ERROR "Could not retrieve OpenApi for requested service" next ctx
      else
        return! Giraffe.HttpStatusCodeHandlers.RequestErrors.NOT_FOUND "Please specify a valid service url in the service query parameter!" next ctx
    }

let dereferencedOpenApiHandler : HttpHandler =
  fun next (ctx : Http.HttpContext) ->
    task {
      let hasService, serviceUris = ctx.Request.Query.TryGetValue "service"
      if hasService then
        let serviceUri = serviceUris.[0]
        let services = openApiServicesAgent.ReadonlyState
        let registeredService =
          Map.tryFind (OpenApiUrl serviceUri) services
          |> Option.bind (fun serviceIndexingStatus ->
              match serviceIndexingStatus.Status with
               | OpenApiServicesAgent.InProgress -> None
               | OpenApiServicesAgent.Indexed serviceInfo -> Some serviceIndexingStatus
               | OpenApiServicesAgent.Reindexing serviceInfo -> Some serviceIndexingStatus
               | OpenApiServicesAgent.Failed _ -> None)
        match registeredService with
        | Some { DereferencedOpenApi = Some (OpenApiFixedContextEntry dereferencedOpenApi) } ->
          let prettyJsonResult = prettifyJson dereferencedOpenApi
          match prettyJsonResult with
          | Ok prettyJson ->
            ctx.SetHttpHeader "Content-Type" "application/json"
            return! Giraffe.HttpStatusCodeHandlers.Successful.ok (text prettyJson) next ctx
          | Error err ->
            return! Giraffe.HttpStatusCodeHandlers.Successful.ok (text dereferencedOpenApi) next ctx
        | _ ->
          return! Giraffe.HttpStatusCodeHandlers.ServerErrors.INTERNAL_ERROR "Could not retrieve OpenApi for requested service" next ctx
      else
        return! Giraffe.HttpStatusCodeHandlers.RequestErrors.NOT_FOUND "Please specify a valid service url in the service query parameter!" next ctx
    }
