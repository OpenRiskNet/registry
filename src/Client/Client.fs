module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Thoth.Json

open Fulma


open Orn.Registry
open System.Collections.Specialized
open Orn.Registry.Shared
open Fable.Import
open Fable.PowerPack
open System.Net
open Fable.FontAwesome
open Fulma
open Fulma
open System.Drawing
open Fable.Core



type IKeycloakPromise<'returnType> =
  abstract success : ('returnType -> Unit) -> IKeycloakPromise<'returnType>
  abstract error : (obj -> Unit) -> IKeycloakPromise<'returnType>

type IKeycloakPromiseWrapper<'returnType> =
  abstract promise : IKeycloakPromise<'returnType>

type IKeycloak =
  abstract init : Unit -> IKeycloakPromise<bool>
  abstract login : obj -> Unit
  abstract token : string
  abstract subject : string

type OntologySearchTerm =
  { Text : string
    OntologyTerm : string option
    TermSuggestions : string list
  }

type SearchStatus =
  | NotStarted
  | Loading
  | Results of Shared.ServiceSqarqlQueryResult list

type ServiceList =
  | ServicesLoading
  | ServicesError of string
  | Services of Shared.ActiveServices

type ActiveTab =
  | ServicesTab
  | SparqlQueryTab
  | ExternalServices

let AllTabs = [ ServicesTab ; SparqlQueryTab ; ExternalServices ]

let tabToLabel = function
| ServicesTab -> "Services"
| SparqlQueryTab -> "SparQL query"
| ExternalServices -> "External services"

type AuthToken = AuthToken of string

type LoginInfo =
  { Token : AuthToken
    UserId : string }

type AppModel =
  { Services : ServiceList
    InputSearchTerm : OntologySearchTerm
    OutputSearchTerm : OntologySearchTerm
    SparqlQuery : string
    SparqlResults : SparqlResultsForServices option
    ActiveTab : ActiveTab
    ExternalServiceTextFieldContent : string
    SelectedSparqlService : string
    LoginInfo : LoginInfo
  }

type Model =
| Authenticating
| LoggedIn of AppModel
| LoginError of string

type AppMsg =
| Refresh of Result<Shared.ActiveServices, exn>
| RunSparqlQuery
| SparqlQueryFinished of Result<SparqlResultsForServices, exn>
| Awake
| QueryChanged of string
| TabChanged of ActiveTab
| AddExternalService
| RemoveExternalService of string
| ExternalServiceTextFieldChanged of string
| AddExternalServiceRequestCompleted of Result<Response, exn>
| RemoveExternalServiceRequestCompleted of Result<Response, exn>
| SparqlSerivceSelected of string

type Msg =
| AppMessage of AppMsg
| KeycloakInit of Result<LoginInfo, obj>

let buildUrl (baseString : string) (parameters : (string * string list) list) : string =
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

let authHeader (AuthToken authToken) =
  HttpRequestHeaders.Authorization (sprintf "Bearer: %s" authToken)

let refresh token =
    Cmd.ofPromise
      (fetchAs<ActiveServices> "/api/services" (Decode.Auto.generateDecoder()))
      [ Fetch.requestHeaders [ authHeader token ]]
      (Ok >> Refresh)
      (Error >> Refresh)

let runSparqlQuery selectedSparqlService query =
    let queryParams =
      [("query", [query])]
      @
      if selectedSparqlService = ""
      then []
      else [("service", [selectedSparqlService])]
    let url = buildUrl "/api/sparql" queryParams
    JS.console.log("Running query with url: ", [url])
    Cmd.ofPromise
      (fetchAs<SparqlResultsForServices> url (Decode.Auto.generateDecoder()))
      []
      (Ok >> SparqlQueryFinished)
      (Error >> SparqlQueryFinished)

let addExternalService token service =
    Cmd.ofPromise
      (fetch (buildUrl "/api/external-services" [("service", [service])]))
      [ RequestProperties.Method HttpMethod.POST
        Fetch.requestHeaders [ authHeader token ]]
      (Ok >> AddExternalServiceRequestCompleted)
      (Error >> AddExternalServiceRequestCompleted)

let removeExternalService token service =
    Cmd.ofPromise
      (fetch (buildUrl "/api/external-services" [("service", [service])]))
      [ RequestProperties.Method HttpMethod.DELETE
        Fetch.requestHeaders [ authHeader token ]]
      (Ok >> AddExternalServiceRequestCompleted)
      (Error >> AddExternalServiceRequestCompleted)

let sleep =
    Cmd.ofPromise
      (fun _ -> Fable.PowerPack.Promise.sleep 20000)
      ()
      (fun _ -> Awake)
      (fun _ -> Awake)


// TODO: Try to make this query work
let initialQuery = """PREFIX orn: <http://openrisknet.org/schema#>

# A very simple query the title of the Api:
SELECT ?title
WHERE {
?tool orn:info ?info.
?info orn:title ?title
}

# Another example query to get all tripples that could be resolved using the JsonLd context
# SELECT * {?s ?p ?o}
"""

let moreInterestingQuery = """PREFIX orn: <http://openrisknet.org/schema#>

SELECT * { ?s1 <orn:paths> ?o1 .
?o1 (<orn:blank>|!<orn:blank>)* ?o2 .
?o2 <http://semanticscience.org/resource/CHEMINF_000018> ?o}
"""

let ornServicesTestValues =
  [ { K8sService = {Name = "Test"; ServicePorts=[|8080|]}
      OpenApiServiceInformation =
        { Description = """Jaqpot v4 (Quattro) is the 4th version of a YAQP, a RESTful web platform which can be used to train machine learning models and use them to obtain toxicological predictions for given chemical compounds or engineered nano materials. Jaqpot v4 has integrated read-across, optimal experimental design, interlaboratory comparison, biokinetics and dose response modelling functionalities. The project is developed in Java8 and JEE7 by the <a href="http://www.chemeng.ntua.gr/labs/control_lab/"> Unit of Process Control and Informatics in the School of Chemical Engineering </a> at the <a href="https://www.ntua.gr/en/"> National Technical University of Athens.</a> """
          Endpoints = ["/algorithm" ; "/api/api.json"; "/algorithm/DecisionStump/bagging" ]
          OpenApiUrl = OpenApiUrl "http://someserivce/openapi.json"
          Name = "Jaqpot"
          RetrievedAt = System.DateTimeOffset.UtcNow
          }
    }]

let testServices =
  Services {PlainK8sServices = []; OrnServices = ornServicesTestValues; ExternalOrnServices = []; ExternalServices = [] ; Messages = []}


let testSparqlResult =
  [| { ServiceName = "Test"
       OpenApiUrl = OpenApiUrl "http://bla.com/openapi"
       Result = BindingResult { Variables = ["subject"; "predicate"; "object"]; ResultValues = [["Lazar REST Service^^http://www.w3.org/2001/XMLSchema#string"; "identity"; "Lazar REST Service^^http://www.w3.org/2001/XMLSchema#string"]]}} |]


let init (keycloak : IKeycloak) : Model * Cmd<Msg> =


  let model =
    Authenticating

  model, Cmd.none


let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  let model', cmd =
    match msg with
    | AppMessage appmsg ->
      match model with
      | Authenticating
      | LoginError _ ->
        model, Cmd.none
      | LoggedIn appModel ->
        let newAppModel, cmd =
          match appmsg with
          | Refresh (Ok services) -> { appModel with Services = Services services }, sleep
          | Refresh (Error err) -> { appModel with Services = ServicesError (err.ToString()) }, sleep
          | SparqlQueryFinished (Ok results) -> { appModel with SparqlResults = Some results}, Cmd.none
          | SparqlQueryFinished (Error err) ->
              JS.console.log("Error when running sparql query!", [err])
              { appModel with SparqlResults = None}, Cmd.none
          | RunSparqlQuery -> { appModel with SparqlResults = None}, runSparqlQuery appModel.SelectedSparqlService appModel.SparqlQuery
          | QueryChanged query ->
            JS.console.log("Query updated", [query])
            { appModel with SparqlQuery = query}, Cmd.none
          | Awake -> appModel, refresh appModel.LoginInfo.Token
          | TabChanged newTab -> { appModel with ActiveTab = newTab}, Cmd.none
          | ExternalServiceTextFieldChanged newText ->
              { appModel with ExternalServiceTextFieldContent = newText }, Cmd.none
          | AddExternalService ->
              appModel, addExternalService appModel.LoginInfo.Token appModel.ExternalServiceTextFieldContent
          | RemoveExternalService service ->
              appModel, removeExternalService appModel.LoginInfo.Token service
          | AddExternalServiceRequestCompleted (Ok _) -> appModel, Cmd.none
          | AddExternalServiceRequestCompleted (Error err) ->
            Fable.Import.JS.console.log("Error:", [err])
            appModel, Cmd.none
          | RemoveExternalServiceRequestCompleted (Ok _) -> appModel, Cmd.none
          | RemoveExternalServiceRequestCompleted (Error err) ->
            Fable.Import.JS.console.log("Error:", [err])
            appModel, Cmd.none
          | SparqlSerivceSelected newSelection -> { appModel with SelectedSparqlService = newSelection }, Cmd.none
        LoggedIn newAppModel, Cmd.map AppMessage cmd
    | KeycloakInit (Ok loginInfo) ->
      let localDebugMode = false
      let initialServices, initialCommand, initialResults =
        if localDebugMode
        then testServices, Cmd.none, Some testSparqlResult
        else ServicesLoading, refresh loginInfo.Token, None

      let newappModel =
        { Services = initialServices
          InputSearchTerm =
            { Text = ""
              OntologyTerm = None
              TermSuggestions = [] }
          OutputSearchTerm =
            { Text = ""
              OntologyTerm = None
              TermSuggestions = [] }
          SparqlQuery = initialQuery
          SparqlResults = initialResults
          ActiveTab = ServicesTab
          ExternalServiceTextFieldContent = ""
          SelectedSparqlService = ""
          LoginInfo = loginInfo
        }

      LoggedIn newappModel, Cmd.map AppMessage initialCommand
    | KeycloakInit (Error err) ->
      Fable.Import.JS.console.log("Error: ", err)

      model, Cmd.none

  model', cmd


let appView (model : AppModel) (dispatch : AppMsg -> unit) =
  let tabContent =
    match model.ActiveTab with
    | SparqlQueryTab ->
        let results =
          match model.SparqlResults with
          | None -> []
          | Some results ->
              results
              |> Array.toList
              |> List.collect (fun result ->
                    [   div [ ]
                           ([ h5 [] [ str result.ServiceName]
                              div [ ] [ a [ Href (result.OpenApiUrl.ToString()); Target "_blank" ] [ str "View OpenApi →" ]]
                            ]
                            @
                            match result.Result with
                            | BooleanResult resultvalue ->
                                [ div [] [ (if resultvalue then str "True" else str "False")] ]
                            | BindingResult bindingresult ->
                                let headers = tr [] (bindingresult.Variables |> List.map (fun variable -> th [  ] [str variable] ))
                                let rows =
                                  bindingresult.ResultValues
                                  |> List.map (fun resultrow -> tr [] (resultrow |> List.map (fun cell -> td [] [ str cell])))

                                [ div [ ClassName "container"]
                                    [
                                      table [ ClassName "table"]
                                        [ thead [] [headers]
                                          tbody [] rows
                                        ]
                                    ]
                                ]
                            | NoResult -> []
                           )
                    ]


                  )
        [ h3  [] [ str "Custom SparQL query" ]
          div [ ClassName "form-group" ] <|
            [ textarea [ Rows 10; Class "form-control"; OnChange (fun e -> dispatch (QueryChanged e.Value)) ; DefaultValue (model.SparqlQuery)] []
            ]
            @
            match model.Services with
            | ServicesLoading -> []
            | ServicesError err -> [ ]
            | Services {ExternalOrnServices = externalServices; OrnServices = ornServices; Messages = messages } ->
              [ Fulma.Columns.columns []
                  [ Fulma.Column.column [ Column.Option.Width(Screen.All, Column.IsOneThird) ] [ label [] [ str "Query only this service (optional): " ] ]
                    Fulma.Column.column []
                      [
                        Fulma.Select.select [ Select.Option.IsFullWidth  ]
                          [ select [ Value model.SelectedSparqlService; OnChange (fun event -> dispatch <| SparqlSerivceSelected event.Value) ]
                              ( [ option [ Value "" ]  [ str "" ] ]
                                @
                                (
                                  List.concat
                                    [ externalServices |> List.map (fun service -> service.OpenApiServiceInformation)
                                      ornServices |> List.map (fun service -> service.OpenApiServiceInformation)
                                    ]
                                  |> List.map (fun service ->
                                        option [ Value service.OpenApiUrl ]  [ str service.Name ] ) )
                                ) ] ]
                  ]
              ]

          button [ ClassName "btn btn-primary"; OnClick (fun _ -> dispatch RunSparqlQuery) ] [ str "Search" ]
          h3 [] [ str "Results" ]
        ]
        @ (if List.isEmpty results then [  str "No results" ] else results)
    | ExternalServices ->
        match model.Services with
        | ServicesLoading -> [ p [] [str "Loading ..."] ]
        | ServicesError err -> [ p [] [str ("Error loading services: " + err)] ]
        | Services {ExternalServices = externalServices; Messages = messages } ->
            let externalServiceFragments =
                externalServices
                |> List.map (fun service ->
                      div [ ClassName "row resource-listing__resource" ]
                          [ div [ ClassName "resource__title" ]
                               [ str service ]
                            Button.a [ Button.Option.OnClick (fun _ -> dispatch <| RemoveExternalService service ) ] [ Fa.i [ Fa.Solid.Trash ] [] ]
                          ]
                )
            let feedbackMessages =
                messages
                |> List.map (fun feedbackMessage ->
                      div [  ]
                          [( match feedbackMessage.Feedback with
                             | OpenApiDownloadFailed (OpenApiUrl url) -> str (sprintf "Downloading OpenAPI failed from URL: %s" url)
                             | OpenApiParsingFailed (OpenApiUrl url, openapiMessage) -> str (sprintf "Parsing OpenAPI failed (from URL: %s) with message: %s" url openapiMessage)
                             | JsonLdContextMissing (OpenApiUrl url) -> str (sprintf "Json-LD context missing in OpenAPI definition at URL: %s" url)
                             | JsonLdParsingError (OpenApiUrl url, jsonldMessage) -> str (sprintf "Json-LD parsing error (from URL: %s) with message: %s" url jsonldMessage)
                          )]
                  )

            [ h3  [] [ str "Add a new external service" ]
              Fulma.Text.p [ GenericOption.Modifiers [ Fulma.Modifier.TextSize(Screen.All, TextSize.Is7) ] ] [ str "Please beware that external services are not persisted at the moment (i.e. you have to add them again if the registry is restarted)"]

              Fulma.Columns.columns []
                [
                  Column.column [ Column.Option.Width(Screen.All, Column.IsFourFifths) ]
                   [ Fulma.Input.text
                      [ Input.Option.ValueOrDefault model.ExternalServiceTextFieldContent
                        Input.Option.OnChange (fun event -> dispatch <| ExternalServiceTextFieldChanged event.Value )
                        Input.Placeholder "Absolute URL of OpenRiskNet annotated OpenAPI definition"
                       ]
                   ]
                  Column.column []
                   [ Fulma.Button.button [ Button.OnClick (fun _ -> dispatch AddExternalService) ] [ str "Add service"]
                   ]

                ]
              h3  [] [ str "Currently registered external services" ]
            ]
            @
            externalServiceFragments
            @
            [
              h3  [] [ str "Recent registry messages: " ]
              div [] feedbackMessages
            ]
    | ServicesTab ->
        match model.Services with
        | ServicesLoading -> [ p [] [str "Loading ..."] ]
        | ServicesError err -> [ p [] [str ("Error loading services: " + err)] ]
        | Services {PlainK8sServices = k8sServices; OrnServices = ornServices; ExternalOrnServices = externalServices; Messages = messages} ->
            // TODO: render external services
            let plainK8sFragments =
                k8sServices
                |> List.map (fun app ->
                      div [ ClassName "media" ; Style [ Border "1px solid lightgrey" ; Padding "1em" ] ]
                          [ div [ ClassName "media-body" ]
                              [ h5 [ ClassName "mt-0" ]
                                   [ str app.Name ]
                              ]
                          ])
            let ornServiceFragments =
                ornServices
                |> List.map (fun app ->
                      let swaggerUrl = app.OpenApiServiceInformation.OpenApiUrl.Unwrap()
                      let swaggerUiLink = sprintf "/swaggerui?service=%s" (swaggerUrl |> Fable.Import.JS.encodeURIComponent)
                      let rawOpenApiLink = sprintf "/openapi-raw?service=%s" (swaggerUrl |> Fable.Import.JS.encodeURIComponent)
                      let dereferencedOpenApiLink = sprintf "/openapi-dereferenced?service=%s" (swaggerUrl |> Fable.Import.JS.encodeURIComponent)
                      div [ ClassName "col-md-6" ]
                          [ div [ ClassName "services-listing__service" ]
                              [ div [ ClassName "service__name" ]
                                   [ str app.OpenApiServiceInformation.Name ]
                                div [ ClassName "service__info service__info-item"]
                                  [ div [ ClassName "service__info-label"] [ str "Indexed at "]
                                    div [ ClassName "service__info-value"] [ str (app.OpenApiServiceInformation.RetrievedAt.ToString("u")) ]
                                  ]
                                div [ ClassName "service__description"] [ str app.OpenApiServiceInformation.Description ]
                                br []
                                div [ ClassName "service__info" ]
                                  ( app.OpenApiServiceInformation.Endpoints
                                    |> List.map (fun endpoint -> div [ ClassName "service__info-item" ] [ str endpoint ]) )
                                div [ ClassName "service__more-links"]
                                  [ a [ Href (rawOpenApiLink); Target "_blank" ] [ str "View raw OpenApi →" ]
                                    br []
                                    a [ Href (dereferencedOpenApiLink); Target "_blank" ] [ str "View dereferenced OpenApi →" ]
                                    br []
                                    a [ Href (swaggerUiLink); Target "_blank" ] [ str "View SwaggerUI →" ]
                                  ]
                              ]
                          ]
                )
            let externalServiceFragments =
                externalServices
                |> List.map (fun app ->
                      let swaggerUrl = app.OpenApiServiceInformation.OpenApiUrl.Unwrap()
                      let swaggerUiLink = sprintf "/openapi?service=%s" (swaggerUrl |> Fable.Import.JS.encodeURIComponent)
                      div [ ClassName "col-md-6" ]
                          [ div [ ClassName "services-listing__service" ]
                              [ div [ ClassName "service__name" ]
                                   [ str app.OpenApiServiceInformation.Name ]
                                div [ ClassName "service__description"] [ str app.OpenApiServiceInformation.Description ]
                                br []
                                div [ ClassName "service__info" ]
                                  ( app.OpenApiServiceInformation.Endpoints
                                    |> List.map (fun endpoint -> div [ ClassName "service__info-item" ] [ str endpoint ]) )
                                div [ ClassName "service__more-links"] [ a [ Href (swaggerUiLink); Target "_blank" ] [ str "View OpenApi →" ]]
                              ]
                          ]
                )
            let feedbackMessages =
                messages
                |> List.map (fun feedbackMessage ->
                      div [  ]
                          [( match feedbackMessage.Feedback with
                             | OpenApiDownloadFailed (OpenApiUrl url) -> str (sprintf "Downloading OpenAPI failed from URL: %s" url)
                             | OpenApiParsingFailed (OpenApiUrl url, openapiMessage) -> str (sprintf "Parsing OpenAPI failed (from URL: %s) with message: %s" url openapiMessage)
                             | JsonLdContextMissing (OpenApiUrl url) -> str (sprintf "Json-LD context missing in OpenAPI definition at URL: %s" url)
                             | JsonLdParsingError (OpenApiUrl url, jsonldMessage) -> str (sprintf "Json-LD parsing error (from URL: %s) with message: %s" url jsonldMessage)
                          )]
                  )

            [ h3  [] [ str "OpenRiskNet services running in the VRE" ]
              div [ ClassName "row services-listing" ] ornServiceFragments
              h3  [] [ str "External services with OpenRiskNet annotation" ]
              div [ ClassName "row services-listing" ] externalServiceFragments
              h3  [] [ str "Kubernetes services (debug view)" ]
              div [] plainK8sFragments
              h3  [] [ str "Recent registry messages: " ]
              div [] feedbackMessages
            ]

  let renderTabs options labelFn activeTab message =
    Tabs.tabs [ ]
        (options
         |> List.map (fun option ->
                          Tabs.tab
                              (if activeTab = option then [ Tabs.Tab.IsActive true] else [])
                              [ a [ OnClick (fun _ -> dispatch (message option)) ] [ str (labelFn option) ] ] ) )
  div []
    [ renderTabs AllTabs tabToLabel model.ActiveTab TabChanged
      div [] tabContent
    ]

let view (model : Model) (dispatch : Msg -> unit) =
  match model with
  | Authenticating ->
    div [] [ str "Authenticating..."]
  | LoggedIn appModel ->
    appView appModel (AppMessage >> dispatch)
  | LoginError error ->
    div [] [ str "Error during login: "; str error ]


open Fable.Core
open Fable.Core.JsInterop


[<Emit("keycloak")>]
let keycloak : IKeycloak = jsNative

let keycloakInit (keycloak : IKeycloak) initialModel =
  let sub dispatch =
    let sendInitOk(isAuthenticated : bool) =
      if not isAuthenticated then
        keycloak.login() |> ignore
      else
        dispatch(KeycloakInit (Ok { Token = AuthToken keycloak.token; UserId = keycloak.subject}))
    let sendInitError(err) =
      dispatch(KeycloakInit(Error(err)))
    keycloak.init().success(sendInitOk).error(sendInitError) |> ignore
  Cmd.ofSub sub


let isAuthenticated : bool = (keycloak?authenticated)

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withSubscription (keycloakInit keycloak)
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.runWith keycloak
