module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared
open System.Collections.Specialized
open Fable.PowerPack


type OntologySearchTerm =
  { Text : string
    OntologyTerm : string option
    TermSuggestions : string list
  }

type SearchStatus =
  | NotStarted
  | Loading
  | Results of ServiceSqarqlQueryResult list

type ServiceList =
  | ServicesLoading
  | ServicesError of string
  | Services of Service list

type Model =
  { Services : ServiceList
    InputSearchTerm : OntologySearchTerm
    OutputSearchTerm : OntologySearchTerm
  }

type Msg =
| Refresh of Result<Service list, exn>
| Awake

module Server =

  open Shared
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
  let serviceFragments =
    match model.Services with
    | ServicesLoading -> [ p [] [str "Loading ..."] ]
    | ServicesError err -> [ p [] [str ("Error loading services: " + err)] ]
    | Services services ->
        services
        |> List.map (fun app ->
              div [ ClassName "media" ; Style [ Border "1px solid lightgrey" ; Padding "1em" ] ]
                  [ div [ ClassName "media-body" ]
                      [ h5 [ ClassName "mt-0" ]
                           [ str app.Name ]
                        str app.Name
                        ul [ Style [ Margin "1em"; ListStyleType "none"] ]
                          [ li [ Style [Display "inline-block"] ] [ icon "fas fa-book fa-2x" ]
                            li [ Style [Display "inline-block"] ] [ icon "fas fa-code fa-2x" ]
                          ]
                      ]
                  ]
        )


  div []
    [ h3  [] [ str "Active services" ]
      ul []
         serviceFragments
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
