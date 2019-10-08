module Orn.Registry.Client.View

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Orn.Registry.Client.ReactMarkdown

open Fulma


open Orn.Registry.Shared
open Fable.FontAwesome

open Orn.Registry.Client.Types

let trashIcon =
  i [ ClassName "fa fa-trash" ] []

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
          | Services {ExternalServices = externalServices; ExternalServiceLists = externalServiceLists; Messages = messages } ->
              let externalServiceFragments =
                  externalServices
                  |> List.map (fun service ->
                        div [ ClassName "row resource-listing__resource" ]
                            [ div [ ClassName "resource__title" ]
                                 [ str service ]
                              Button.a [ Button.Option.OnClick (fun _ -> dispatch <| RemoveExternalService service ) ] [ trashIcon ]
                            ]
                  )
              let externalServiceListFragments =
                  externalServiceLists
                  |> List.map (fun service ->
                        div [ ClassName "row resource-listing__resource" ]
                            [ div [ ClassName "resource__title" ]
                                 [ str service.ListUrl ]
                              Button.a [ Button.Option.OnClick (fun _ -> dispatch <| RemoveExternalServiceList service.ListUrl ) ] [ trashIcon ]
                            ]
                  )
              let feedbackMessages =
                  messages
                  |> List.map (fun feedbackMessage ->
                        div [  ]
                            [( match feedbackMessage.Feedback with
                               | OpenApiDownloadFailed (OpenApiUrl url) -> str (sprintf "Downloading OpenAPI failed from URL: %s" url)
                               | OpenApiParsingFailed (OpenApiUrl url, openapiMessage) -> str (sprintf "Parsing OpenAPI failed (from URL: %s) with message: %s" url openapiMessage)
                               | ListDownloadFailed (url) -> str (sprintf "Downloading or processing of this list failed: %s" url)
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
              [ h3  [] [ str "Add a new external dynamic list of services" ]
                Fulma.Text.p [ GenericOption.Modifiers [ Fulma.Modifier.TextSize(Screen.All, TextSize.Is7) ] ] [ str "Please beware that external lists of services are not persisted at the moment (i.e. you have to add them again if the registry is restarted)"]

                Fulma.Columns.columns []
                  [
                    Column.column [ Column.Option.Width(Screen.All, Column.IsFourFifths) ]
                     [ Fulma.Input.text
                        [ Input.Option.ValueOrDefault model.ExternalServiceListTextFieldContent
                          Input.Option.OnChange (fun event -> dispatch <| ExternalServiceListTextFieldChanged event.Value )
                          Input.Placeholder "Absolute URL of a service list (json array of url strings of OpenRiskNet annotated OpenAPI definitions)"
                         ]
                     ]
                    Column.column []
                     [ Fulma.Button.button [ Button.OnClick (fun _ -> dispatch AddExternalServiceList) ] [ str "Add service list"]
                     ]

                  ]
                h3  [] [ str "Currently registered external service lists" ]
              ]
              @
              externalServiceListFragments
              @
              [
                h3  [] [ str "Recent registry messages: " ]
                div [] feedbackMessages
              ]
      | ServicesTab ->
          match model.Services with
          | ServicesLoading -> [ p [] [str "Loading ..."] ]
          | ServicesError err -> [ p [] [str ("Error loading services: " + err)] ]
          | Services {PlainK8sServices = k8sServices; OrnServices = ornServices; ExternalOrnServices = externalServices; ExternalServiceLists = externalServiceLists; Messages = messages} ->
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
              let renderAppFragment (app : OpenApiServiceInformation) =
                  let swaggerUrl = app.OpenApiUrl.Unwrap()
                  let swaggerUiLink = sprintf "/swaggerui?service=%s" (swaggerUrl |> Fable.Import.JS.encodeURIComponent)
                  let rawOpenApiLink = sprintf "/openapi-raw?service=%s" (swaggerUrl |> Fable.Import.JS.encodeURIComponent)
                  let dereferencedOpenApiLink = sprintf "/openapi-dereferenced?service=%s" (swaggerUrl |> Fable.Import.JS.encodeURIComponent)
                  div [ ClassName "col-md-6" ]
                      [ div [ ClassName "services-listing__service"; Style [OverflowX "scroll"] ]
                          [ div [ ClassName "service__name" ]
                               [ str app.Name ]
                            div [ ClassName "service__info service__info-item"]
                              [ div [ ClassName "service__info-label"] [ str "Indexed at "]
                                div [ ClassName "service__info-value"] [ str (app.RetrievedAt.ToString("yyyy-MMM-dd HH:mm:ss")) ]
                              ]
                            div [ ClassName "service__description"] [ reactMarkdown [ Source app.Description ] ]
                            br []
                            div [ ClassName "service__info" ]
                              ( app.Endpoints
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
              let ornServiceFragments =
                  ornServices
                  |> List.map (fun app -> renderAppFragment app.OpenApiServiceInformation)
              let externalServiceFragments =
                  externalServices
                  |> List.map (fun app -> renderAppFragment app.OpenApiServiceInformation)
              let externalServiceListFragments =
                  externalServiceLists
                  |> List.map (fun list ->
                        let services =
                          list.Services
                          |> List.map renderAppFragment
                        div []
                          [ h4 [] [ str list.ListUrl ]
                            div [ ClassName "row services-listing" ] services
                          ]
                  )
              let feedbackMessages =
                  messages
                  |> List.map (fun feedbackMessage ->
                        div [  ]
                            [( match feedbackMessage.Feedback with
                               | OpenApiDownloadFailed (OpenApiUrl url) -> str (sprintf "Downloading OpenAPI failed from URL: %s" url)
                               | OpenApiParsingFailed (OpenApiUrl url, openapiMessage) -> str (sprintf "Parsing OpenAPI failed (from URL: %s) with message: %s" url openapiMessage)
                               | ListDownloadFailed (url) -> str (sprintf "Downloading or processing of this list failed: %s" url)
                               | JsonLdContextMissing (OpenApiUrl url) -> str (sprintf "Json-LD context missing in OpenAPI definition at URL: %s" url)
                               | JsonLdParsingError (OpenApiUrl url, jsonldMessage) -> str (sprintf "Json-LD parsing error (from URL: %s) with message: %s" url jsonldMessage)
                            )]
                    )

              [ h3  [] [ str "OpenRiskNet services running in the VRE" ]
                div [ ClassName "row services-listing" ] ornServiceFragments
                h3  [] [ str "External services with OpenRiskNet annotation" ]
                div [ ClassName "row services-listing" ] externalServiceFragments
                h3  [] [ str "External service lists" ]
                div [] externalServiceListFragments
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
