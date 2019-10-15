module Orn.Registry.Client.Commands

open Elmish

open Fable.PowerPack.Fetch

open Thoth.Json

open Orn.Registry.Shared
open Fable.Import
open Fable.PowerPack

open Orn.Registry.Client.Types

let buildUrl (baseString: string) (parameters: (string * string list) list): string =
    let allUsedParams = parameters |> List.filter (fun (_, values) -> not (List.isEmpty values))
    let encodeUri str = System.Uri.EscapeDataString(str)

    let paramMapFn (key, values) =
        let concatenatedVals = String.concat "," values |> encodeUri
        encodeUri key + "=" + concatenatedVals

    let parameters = (List.map paramMapFn allUsedParams)

    let paramsString =
        match parameters with
        | [] -> ""
        | _ :: _ -> "?" + (String.concat "&" parameters)
    baseString + paramsString

let authHeader (AuthToken authToken) = HttpRequestHeaders.Authorization(sprintf "Bearer %s" authToken)

let refresh token =
    Cmd.ofPromise (fetchAs<ActiveServices> "/api/services" (Decode.Auto.generateDecoder()))
        [ Fetch.requestHeaders [ authHeader token ] ] (Ok >> Refresh) (Error >> Refresh)

let runSparqlQuery selectedSparqlService query =
    let queryParams =
        [ ("query", [ query ]) ] @ if selectedSparqlService = "" then []
                                   else [ ("service", [ selectedSparqlService ]) ]

    let url = buildUrl "/api/sparql" queryParams
    JS.console.log ("Running query with url: ", [ url ])
    Cmd.ofPromise (fetchAs<SparqlResultsForServices> url (Decode.Auto.generateDecoder())) [] (Ok >> SparqlQueryFinished)
        (Error >> SparqlQueryFinished)

let addExternalService token service =
    Cmd.ofPromise (fetch (buildUrl "/api/external-services" [ ("service", [ service ]) ]))
        [ RequestProperties.Method HttpMethod.POST
          Fetch.requestHeaders [ authHeader token ] ] (Ok >> AddExternalServiceRequestCompleted)
        (Error >> AddExternalServiceRequestCompleted)

let removeExternalService token service =
    Cmd.ofPromise (fetch (buildUrl "/api/external-services" [ ("service", [ service ]) ]))
        [ RequestProperties.Method HttpMethod.DELETE
          Fetch.requestHeaders [ authHeader token ] ] (Ok >> AddExternalServiceRequestCompleted)
        (Error >> AddExternalServiceRequestCompleted)

let addExternalServiceList token list =
    Cmd.ofPromise (fetch (buildUrl "/api/external-service-lists" [ ("list", [ list ]) ]))
        [ RequestProperties.Method HttpMethod.POST
          Fetch.requestHeaders [ authHeader token ] ] (Ok >> AddExternalServiceRequestCompleted)
        (Error >> AddExternalServiceRequestCompleted)

let removeExternalServiceList token list =
    Cmd.ofPromise (fetch (buildUrl "/api/external-service-lists" [ ("list", [ list ]) ]))
        [ RequestProperties.Method HttpMethod.DELETE
          Fetch.requestHeaders [ authHeader token ] ] (Ok >> AddExternalServiceRequestCompleted)
        (Error >> AddExternalServiceRequestCompleted)

let sleep = Cmd.ofPromise (fun _ -> Fable.PowerPack.Promise.sleep 7000) () (fun _ -> Awake) (fun _ -> Awake)
