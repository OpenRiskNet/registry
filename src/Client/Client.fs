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

type Model =
  { Services : ServiceList
    InputSearchTerm : OntologySearchTerm
    OutputSearchTerm : OntologySearchTerm
    SparqlQuery : string
    SparqlResults : SparqlResultsForServices option
  }

type Msg =
| Refresh of Result<Shared.ActiveServices, exn>
| Awake

let refresh =
    Cmd.ofPromise
      (fetchAs<ActiveServices> "/api/services" (Decode.Auto.generateDecoder()))
      []
      (Ok >> Refresh)
      (Error >> Refresh)

let sleep =
    Cmd.ofPromise
      (fun _ -> Fable.PowerPack.Promise.sleep 2000)
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
    }

  model, refresh

let icon iconclasses =
  i [ ClassName iconclasses; Style [MarginRight "0.5em"] ] []

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  let model', cmd =
    match msg with
    | Refresh (Ok services) -> { model with Services = Services services }, sleep
    | Refresh (Error err) -> { model with Services = ServicesError (err.ToString()) }, sleep
    | Awake -> model, refresh

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
  let serviceContent =
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


  div [] serviceContent

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
