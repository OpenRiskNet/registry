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

let AllTabs = [ ServicesTab ; SparqlQueryTab ]

let tabToLabel = function
| ServicesTab -> "Services"
| SparqlQueryTab -> "SparQL query"

type Model =
  { Services : ServiceList
    InputSearchTerm : OntologySearchTerm
    OutputSearchTerm : OntologySearchTerm
    SparqlQuery : string
    SparqlResults : SparqlResultsForServices option
    ActiveTab : ActiveTab
  }

type Msg =
| Refresh of Result<Shared.ActiveServices, exn>
| RunSparqlQuery
| SparqlQueryFinished of Result<SparqlResultsForServices, exn>
| Awake
| QueryChanged of string
| TabChanged of ActiveTab

let refresh =
    Cmd.ofPromise
      (fetchAs<ActiveServices> "/api/services" (Decode.Auto.generateDecoder()))
      []
      (Ok >> Refresh)
      (Error >> Refresh)

let runSparqlQuery query =
    let url = sprintf "/api/sparql?query=%s" (Fable.Import.JS.encodeURIComponent (query))
    Cmd.ofPromise
      (fetchAs<SparqlResultsForServices> url (Decode.Auto.generateDecoder()))
      []
      (Ok >> SparqlQueryFinished)
      (Error >> SparqlQueryFinished)

let sleep =
    Cmd.ofPromise
      (fun _ -> Fable.PowerPack.Promise.sleep 20000)
      ()
      (fun _ -> Awake)
      (fun _ -> Awake)

let init () : Model * Cmd<Msg> =
  let model =
    { Services = ServicesLoading
      InputSearchTerm =
        { Text = ""
          OntologyTerm = None
          TermSuggestions = [] }
      OutputSearchTerm =
        { Text = ""
          OntologyTerm = None
          TermSuggestions = [] }
      SparqlQuery = ""
      SparqlResults = None
      ActiveTab = ServicesTab
    }

  model, refresh

let icon iconclasses =
  i [ ClassName iconclasses; Style [MarginRight "0.5em"] ] []

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  let model', cmd =
    match msg with
    | Refresh (Ok services) -> { model with Services = Services services }, sleep
    | Refresh (Error err) -> { model with Services = ServicesError (err.ToString()) }, sleep
    | SparqlQueryFinished (Ok results) -> { model with SparqlResults = Some results}, Cmd.none
    | SparqlQueryFinished (Error err) ->
        JS.console.log("Error when running sparql query!", [err])
        { model with SparqlResults = None}, Cmd.none
    | RunSparqlQuery -> { model with SparqlResults = None}, runSparqlQuery model.SparqlQuery
    | QueryChanged query ->
      JS.console.log("Query updated", [query])
      { model with SparqlQuery = query}, Cmd.none
    | Awake -> model, refresh
    | TabChanged newTab -> { model with ActiveTab = newTab}, Cmd.none

  model', cmd

let ornServicesTestValues =
  [ { K8sService = {Name = "Test"; ServicePorts=[|8080|]}
      OpenApiServiceInformation =
        { Description = """Jaqpot v4 (Quattro) is the 4th version of a YAQP, a RESTful web platform which can be used to train machine learning models and use them to obtain toxicological predictions for given chemical compounds or engineered nano materials. Jaqpot v4 has integrated read-across, optimal experimental design, interlaboratory comparison, biokinetics and dose response modelling functionalities. The project is developed in Java8 and JEE7 by the <a href="http://www.chemeng.ntua.gr/labs/control_lab/"> Unit of Process Control and Informatics in the School of Chemical Engineering </a> at the <a href="https://www.ntua.gr/en/"> National Technical University of Athens.</a> """
          Endpoints = ["/algorithm" ; "/api/api.json"; "/algorithm/DecisionStump/bagging" ]
          OpenApiUrl = OpenApiUrl "http://someserivce/openapi.json"
          Name = "Jaqpot"}
    }]

let testServices =
  Services {PlainK8sServices = []; OrnServices = ornServicesTestValues; Messages = []}

let view (model : Model) (dispatch : Msg -> unit) =
  let tabContent =
    match model.ActiveTab with
    | SparqlQueryTab ->
        let results =
          match model.SparqlResults with
          | None -> []
          | Some results ->
              results
              |> List.collect (fun result ->
                  [ h3 [] [ str result.ServiceName]
                    div [ ClassName "service__more-links"] [ a [ Href (result.OpenApiUrl.ToString()); Target "_blank" ] [ str "View OpenApi →" ]]
                  ]
                  @
                  match result.Result with
                  | BooleanResult resultvalue ->
                      [ div [] [ (if resultvalue then str "True" else str "False")] ]
                  | BindingResult bindingresult ->
                      let headers = tr [] (bindingresult.Variables |> List.map (fun variable -> th [] [str variable] ))
                      let rows =
                        bindingresult.ResultValues
                        |> List.map (fun resultrow -> tr [] (resultrow |> List.map (fun cell -> td [] [ str cell])))

                      [ table []
                          (headers :: rows)
                      ]
                  | NoResult -> []

                  )
        [ h3  [] [ str "Custom SparQL query" ]
          h5 [] [ str "Results" ]
          div [ ClassName "row" ] (if List.isEmpty results then [ str "No results" ] else results)
          div [ ClassName "row" ] [ textarea [ Class "input is-medium"; OnChange (fun e -> dispatch (QueryChanged e.Value)) ; Value (model.SparqlQuery)] []]
          div [ ClassName "control" ]
              [ a
                    [ OnClick (fun _ -> dispatch RunSparqlQuery) ]
                    [ str "Search" ] ]
        ]
    | ServicesTab ->
        match model.Services with
        | ServicesLoading -> [ p [] [str "Loading ..."] ]
        | ServicesError err -> [ p [] [str ("Error loading services: " + err)] ]
        | Services {PlainK8sServices = k8sServices; OrnServices = ornServices; Messages = messages} ->
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
                      div [ ClassName "col-md-6" ]
                          [ div [ ClassName "services-listing__service" ]
                              [ div [ ClassName "service__name" ]
                                   [ str app.K8sService.Name ]
                                div [ ClassName "service__description"] [ str app.OpenApiServiceInformation.Description ]
                                br []
                                div [ ClassName "service__info" ]
                                  ( app.OpenApiServiceInformation.Endpoints
                                    |> List.map (fun endpoint -> div [ ClassName "service__info-item" ] [ str endpoint ]) )
                                div [ ClassName "service__more-links"] [ a [ Href (app.OpenApiServiceInformation.OpenApiUrl.ToString()); Target "_blank" ] [ str "View OpenApi →" ]]
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

            [ h3  [] [ str "Active OpenRiskNet services" ]
              div [ ClassName "row services-listing" ] ornServiceFragments
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

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
