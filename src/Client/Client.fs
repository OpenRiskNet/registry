module Client

open Elmish
open Elmish.React

open Fable.Core
open Fable.Core.JsInterop
open Orn.Registry.Client.Types
open Orn.Registry.Client.Update
open Orn.Registry.Client.View


[<Emit("keycloak")>]
let keycloak: IKeycloak = jsNative

let keycloakInit (keycloak: IKeycloak) initialModel =
    let sub dispatch =
        let sendInitOk (isAuthenticated: bool) =
            if not isAuthenticated then keycloak.login() |> ignore
            else
                dispatch
                    (KeycloakInit
                        (Ok
                            { Token = AuthToken keycloak.token
                              UserId = keycloak.subject }))

        let sendInitError (err) = dispatch (KeycloakInit(Error(err)))
        keycloak.init().success(sendInitOk).error(sendInitError) |> ignore
    Cmd.ofSub sub


let isAuthenticated: bool = (keycloak?authenticated)

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withSubscription (keycloakInit keycloak)
|> Program.withConsoleTrace
|> Program.withHMR
|> Program.withReact "elmish-app"
|> Program.withDebugger
|> Program.runWith keycloak
