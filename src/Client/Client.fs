module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Orn.Registry
open System.Collections.Specialized
open Fable.PowerPack
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
  }

type Msg =
| Refresh of Result<Shared.ActiveServices, exn>
| Awake

module Server =

  open Orn.Registry.Shared
  open Fable.Remoting.Client

  /// A proxy you can use to talk to server directly
  let api : IRegistryProtocol =
    Proxy.remoting<IRegistryProtocol> {
      use_route_builder Route.builder
    }

let refresh =
    Cmd.ofAsync
      Server.api.getCurrentServices
      ()
      (Ok >> Refresh)
      (Error >> Refresh)

let sleep =
    Cmd.ofPromise
      (fun _ -> Promise.sleep 2000)
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

let view (model : Model) (dispatch : Msg -> unit) =
  let serviceContent =
    match model.Services with
    | ServicesLoading -> [ p [] [str "Loading ..."] ]
    | ServicesError err -> [ p [] [str ("Error loading services: " + err)] ]
    | Services {PlainK8sServices = k8sServices; OrnServices = ornServices; Messages = messages} ->
        let plainK8sFragments, ornServiceFragments, feedbackMessages =
          ( k8sServices
            |> List.map (fun app ->
                  div [ ClassName "media" ; Style [ Border "1px solid lightgrey" ; Padding "1em" ] ]
                      [ div [ ClassName "media-body" ]
                          [ h5 [ ClassName "mt-0" ]
                               [ str app.Name ]
                          ]
                      ]
            ),
            ornServices
            |> List.map (fun app ->
                  div [ ClassName "media" ; Style [ Border "1px solid lightgrey" ; Padding "1em" ] ]
                      [ div [ ClassName "media-body" ]
                          [ h4 [ ClassName "mt-0" ]
                               [ str app.K8sService.Name ]
                            str app.OpenApiServiceInformation.Description
                            h5 [ ClassName "mt-0" ]
                               [ str "Endpoints:" ]
                            ul [ ]
                              ( app.OpenApiServiceInformation.Endpoints
                                |> List.map (fun endpoint -> li [  ] [ str endpoint ]) )
                          ]
                      ]
            ),
            messages
            |> List.map (fun feedbackMessage ->
                  div [  ]
                      [( match feedbackMessage.Feedback with
                         | OpenApiDownloadFailed (SwaggerUrl url) -> str (sprintf "Downloading OpenAPI failed from URL: %s" url)
                         | OpenApiParsingFailed (SwaggerUrl url, openapiMessage) -> str (sprintf "Parsing OpenAPI failed (from URL: %s) with message: %s" url openapiMessage)
                         | JsonLdContextMissing (SwaggerUrl url) -> str (sprintf "Json-LD context missing in OpenAPI definition at URL: %s" url)
                         | JsonLdParsingError (SwaggerUrl url, jsonldMessage) -> str (sprintf "Json-LD parsing error (from URL: %s) with message: %s" url jsonldMessage)
                      )]
              )
          )

        [ h3  [] [ str "Active OpenRiskNet services" ]
          div [] ornServiceFragments
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
