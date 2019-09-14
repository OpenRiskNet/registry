module Orn.Registry.Handlers

open Orn.Registry.BasicTypes
open Microsoft.Extensions.Logging

open Giraffe
open Orn.Registry
open Orn.Registry.AgentSetup
open Microsoft.AspNetCore
open FSharp.Control.Tasks.V2.ContextInsensitive
open Orn.Registry.OpenApiProcessing
open Orn.Registry.Shared
open System.Security
open Orn.Registry.Domain

type Email = Email of string

let userEmail (ctx: Http.HttpContext): Email option =
    if not (isNull ctx.User) && ctx.User.Identity.IsAuthenticated then
        ctx.User.FindFirst Claims.ClaimTypes.Email
        |> function
        | null -> None
        | claim ->
            claim.Value
            |> Email
            |> Some
    else None

let requireUserHandler (handler: Email -> HttpHandler): HttpHandler =
    fun next ctx ->
        match userEmail ctx with
        | Some user -> handler user next ctx
        | None ->
            challenge Microsoft.AspNetCore.Authentication.JwtBearer.JwtBearerDefaults.AuthenticationScheme next ctx

let jsonText (str: string) =
    fun (next: HttpFunc) (ctx: Http.HttpContext) ->
        ctx.SetContentType "application/json; charset=utf-8"
        setBodyFromString str next ctx

let getCurrentServicesHandler email: HttpHandler =
    fun next (ctx: Http.HttpContext) ->
        task {
            let logger = ctx.GetLogger()
            let! services = getCurrentServices logger email
            return! Successful.ok (json services) next ctx
        }

let addExternalServiceListHandler email: HttpHandler =
    fun next (ctx: Http.HttpContext) ->
        task {
            let logger = ctx.GetLogger()
            logger.LogInformation("Adding external service list")
            let hasServiceList, list = ctx.Request.Query.TryGetValue "list"
            if not hasServiceList then
                return! RequestErrors.BAD_REQUEST (text "Could not find query parameter 'list'") next ctx
            else
                logger.LogInformation("Adding service list: ", list)
                do listManagementAgent.Post
                    (Orn.Registry.ListManagementAgent.AddNewList(list.[0]))
                ExternalServiceLists <- Set.add list.[0] ExternalServiceLists
                return! Successful.NO_CONTENT next ctx
        }


let removeExternalServiceListHandler email: HttpHandler =
    fun next (ctx: Http.HttpContext) ->
        task {
            let logger = ctx.GetLogger()
            logger.LogInformation("Removing external service list")
            let hasServiceList, list = ctx.Request.Query.TryGetValue "list"
            if not hasServiceList then
                return! RequestErrors.BAD_REQUEST (text "Could not find query parameter 'list'") next ctx
            else
                logger.LogInformation("Removing service list: ", list)
                do listManagementAgent.Post(Orn.Registry.ListManagementAgent.RemoveList(list.[0].ToString()))
                ExternalServiceLists <- Set.remove list.[0] ExternalServiceLists
                return! Successful.NO_CONTENT next ctx
        }

let addExternalServiceHandler email: HttpHandler =
    fun next (ctx: Http.HttpContext) ->
        task {
            let logger = ctx.GetLogger()
            logger.LogInformation("Adding external service")
            let hasService, service = ctx.Request.Query.TryGetValue "service"
            if not hasService then
                return! RequestErrors.BAD_REQUEST (text "Could not find query parameter 'service'") next ctx
            else
                logger.LogInformation("Adding service: ", service)
                do openApiProcessingAgent.Post
                    (IndexNewUrl(OpenApiUrl(service.[0]), None, 60.0<FSharp.Data.UnitSystems.SI.UnitNames.second>))
                ExternalServices <- Set.add service.[0] ExternalServices
                return! Successful.NO_CONTENT next ctx
        }


let removeExternalServiceHandler email: HttpHandler =
    fun next (ctx: Http.HttpContext) ->
        task {
            let logger = ctx.GetLogger()
            logger.LogInformation("Removing external service")
            let hasService, service = ctx.Request.Query.TryGetValue "service"
            if not hasService then
                return! RequestErrors.BAD_REQUEST (text "Could not find query parameter 'service'") next ctx
            else
                logger.LogInformation("Removing service: ", service)
                do openApiProcessingAgent.Post(RemoveUrl(OpenApiUrl(service.[0])))
                ExternalServices <- Set.remove service.[0] ExternalServices
                return! Successful.NO_CONTENT next ctx
        }

let prettifyJson (jsonString: string): Result<string, string> =
    try
        use reader = new System.IO.StringReader(jsonString)
        use writer = new System.IO.StringWriter()
        let jsonReader = new Newtonsoft.Json.JsonTextReader(reader)
        let jsonWriter = new Newtonsoft.Json.JsonTextWriter(writer)
        jsonWriter.Formatting <- Newtonsoft.Json.Formatting.Indented
        jsonWriter.WriteToken(jsonReader)
        writer.ToString() |> Ok
    with ex -> Error(ex.Message)

let runSparqlQueryHandler: HttpHandler =
    fun next (ctx: Http.HttpContext) ->
        task {
            let logger = ctx.GetLogger()
            logger.LogInformation("Processing SparQL query")
            let hasQuery, query = ctx.Request.Query.TryGetValue "query"
            let hasService, serviceValues = ctx.Request.Query.TryGetValue "service"
            if not hasQuery then
                return! RequestErrors.BAD_REQUEST (text "Could not find query parameter 'query'") next ctx
            else
                logger.LogInformation("Query is:", query)
                let service =
                    if hasService then Some(OpenApiUrl serviceValues.[0])
                    else None

                let result = runSparqlQuery logger service (query.[0])
                match result with
                | Ok resultTriplesForServices -> return! Successful.ok (json resultTriplesForServices) next ctx
                | Error error -> return! ServerErrors.INTERNAL_ERROR (text error) next ctx
        }


let swaggerUiHandler: HttpHandler =
    fun next (ctx: Http.HttpContext) ->
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
                | Some { OpenApiRetrievalInformation = Some { OpenApiString = OpenApiRaw rawOpenApi } } ->
                    return! htmlView (Orn.Registry.Views.SwaggerUi.swaggerUi rawOpenApi) next ctx
                | _ -> return! ServerErrors.INTERNAL_ERROR "Could not retrieve OpenApi for requested service" next ctx
            else
                return! RequestErrors.NOT_FOUND "Please specify a valid service url in the service query parameter!"
                            next ctx
        }

let rawOpenApiHandler: HttpHandler =
    fun next (ctx: Http.HttpContext) ->
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
                | Some { OpenApiRetrievalInformation = Some { OpenApiString = OpenApiRaw rawOpenApi } } ->
                    let prettyJsonResult = prettifyJson rawOpenApi
                    match prettyJsonResult with
                    | Ok prettyJson -> return! Successful.ok (jsonText prettyJson) next ctx
                    | Error err -> return! Successful.ok (jsonText (rawOpenApi)) next ctx
                | _ -> return! ServerErrors.INTERNAL_ERROR "Could not retrieve OpenApi for requested service" next ctx
            else
                return! RequestErrors.NOT_FOUND "Please specify a valid service url in the service query parameter!"
                            next ctx
        }

let dereferencedOpenApiHandler: HttpHandler =
    fun next (ctx: Http.HttpContext) ->
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
                | Some { DereferencedOpenApi = Some(OpenApiFixedContextEntry dereferencedOpenApi) } ->
                    let prettyJsonResult = prettifyJson dereferencedOpenApi
                    match prettyJsonResult with
                    | Ok prettyJson -> return! Successful.ok (jsonText prettyJson) next ctx
                    | Error err -> return! Successful.ok (jsonText dereferencedOpenApi) next ctx
                | _ ->
                    return! Giraffe.HttpStatusCodeHandlers.ServerErrors.INTERNAL_ERROR
                                "Could not retrieve OpenApi for requested service" next ctx
            else
                return! Giraffe.HttpStatusCodeHandlers.RequestErrors.NOT_FOUND
                            "Please specify a valid service url in the service query parameter!" next ctx
        }


let getStats : HttpHandler =
    fun next (ctx : Http.HttpContext) ->
        task {
            return! Successful.OK {| PrivateMemoryMb = getUsedMemoryInMb() |} next ctx
        }
