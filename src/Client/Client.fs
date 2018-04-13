module Client

open Elmish
open Elmish.React

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared

type Model =
  { Applications : Application list
    Errors : string list
  }

type Msg =
| Increment
| Decrement
| Init of Result<Application list, exn>

module Server =

  open Shared
  open Fable.Remoting.Client

  /// A proxy you can use to talk to server directly
  let api : IRegistryProtocol =
    Proxy.remoting<IRegistryProtocol> {
      use_route_builder Route.builder
    }

let init () : Model * Cmd<Msg> =
  let model = { Applications = []
                Errors = [] }
  let cmd =
    Cmd.ofAsync
      Server.api.getInitModel
      ()
      (Ok >> Init)
      (Error >> Init)
  model, cmd

let icon iconclasses =
  i [ ClassName iconclasses; Style [MarginRight "0.5em"] ] []

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  let model' =
    match msg with
    | Init (Ok x) -> { model with Applications = x }
    | Init (Error err) -> { model with Errors = err.ToString() :: model.Errors }
  model', Cmd.none

let view (model : Model) (dispatch : Msg -> unit) =
  let applications =
    match model.Applications with
    | [] -> [ p [] [str "Loading ..."] ]
    | apps ->
        apps
        |> List.map (fun app ->
              div [ ClassName "media" ; Style [ Border "1px solid lightgrey" ; Padding "1em" ] ]
                  [ img [ Alt (sprintf "%s logo" app.Name); ClassName "mr-3 img-thumbnail"; Style [ Width "10em" ]; Src app.Logo ]
                    div [ ClassName "media-body" ]
                      [ h5 [ ClassName "mt-0" ]
                           [ str app.Name ]
                        str app.Description
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
         applications
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
