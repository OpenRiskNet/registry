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

let spinnerIcon =
  i [ ClassName "fas fa-circle-notch fa-3x fa-spin" ] [ ]


let exampleQueries (selectedExampleQuery : string) dispatch =
  [ Fulma.Columns.columns []
      [ Fulma.Column.column [ Column.Option.Width(Screen.All, Column.IsOneFifth) ] [ label [] [ str "Load example query:" ] ]
        Fulma.Column.column []
          [
            Fulma.Select.select [ Select.Option.IsFullWidth  ]
              [ select [ Value selectedExampleQuery; OnChange (fun event -> dispatch <| SparqlExampleQuerySelected event.Value) ]
                  ( [ option [ Value "" ]  [ str "" ] ]
                    @
                    (
                      exampleQueries
                      |> List.map (fun (title, value) -> option [ Value title ]  [ str title ] ) ))
                      ] ]
      ]
  ]

let renderMessages messages =
  messages
  |> List.map (fun feedbackMessage ->
        let truncateUrl (url : string) =
            if url.Length > 75 then
                sprintf "%s ... %s" (url.Substring(0, 40)) (url.Substring(url.Length-30, 30))
            else
                url
        let renderUrl (url : string) =
            p [] [ a [ Href url; Target "_blank"] [str (truncateUrl url) ] ]
        let renderMainMessage (msg : string) =
            div [] [ h4 [] [ str msg] ]
        let renderTimestamp (timestamp : System.DateTime) =
            div [ ClassName "news-item__date"] [ str (timestamp.ToString()) ]
        div [ Style [ Margin "1rem" ] ]
            ( match feedbackMessage.Feedback with
               | OpenApiDownloadFailed (OpenApiUrl url) ->
                    [ renderMainMessage "OpenAPI download failed"
                      renderTimestamp feedbackMessage.Timestamp
                      renderUrl url
                      ]
               | OpenApiParsingFailed (OpenApiUrl url, openapiMessage) ->
                    [ yield (renderMainMessage "OpenAPI Parsing failed")
                      yield renderTimestamp feedbackMessage.Timestamp
                      yield renderUrl url
                      yield div [] [ str "Messages: "]
                      yield! (openapiMessage.Split([|'\n'; ';'|]) |> Array.map (fun error -> div [ ClassName "small"  ] [ str error]) |> List.ofArray)
                    ]
               | ListDownloadFailed (url) ->
                    [ renderMainMessage "Downloading or parsing of the list failed"
                      renderTimestamp feedbackMessage.Timestamp
                      renderUrl url
                    ]
               | JsonLdContextMissing (OpenApiUrl url) ->
                    [ renderMainMessage "No Json-ld context ('x-orn-@context') could be found"
                      renderTimestamp feedbackMessage.Timestamp
                      renderUrl url
                    ]
               | JsonLdParsingError (OpenApiUrl url, jsonldMessage) ->
                    [ yield (renderMainMessage "OpenAPI Parsing failed")
                      yield renderTimestamp feedbackMessage.Timestamp
                      yield renderUrl url
                      yield div [] [ str "Messages: "]
                      yield! (jsonldMessage.Split([|'\n'; ';'|]) |> Array.map (fun error -> div [ ClassName "small" ] [ str error]) |> List.ofArray)
                    ]
            )
    )

let appView (model : AppModel) (dispatch : AppMsg -> unit) =
    let tabContent =
      match model.ActiveTab with
      | SparqlQueryTab ->
          let results =
            match model.SparqlResults with
            | NotRequested -> []
            | InFlight -> [ spinnerIcon ]
            | Failed err -> [ div [] [ str err ]]
            | Success results ->
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
          [ h3  [] [ str "Custom SPARQL query" ]
            div [ ClassName "form-group" ] <|
              (exampleQueries model.SelectedExampleSparqlQuery dispatch)
              @
              [ textarea [ Rows 10; Class "form-control"; OnChange (fun e -> dispatch (QueryChanged e.Value)) ; Elmish.React.Helpers.valueOrDefault (model.SparqlQuery)] []
              ]
              @
              match model.Services with
              | NotRequested -> []
              | InFlight -> [ spinnerIcon ]
              | Failed err -> [ div [] [ str err] ]
              | Success {ExternalOrnServices = externalServices; OrnServices = ornServices; Messages = messages } ->
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
          | NotRequested -> []
          | InFlight -> [ spinnerIcon ]
          | Failed err -> [ p [] [str ("Error loading services: " + err)] ]
          | Success {ExternalServices = externalServices; ExternalServiceLists = externalServiceLists; Messages = messages } ->
              let externalServiceFragments =
                  externalServices
                  |> List.map (fun service ->
                        div [ ClassName "media" ]
                            [ Button.a [ Button.Option.OnClick (fun _ -> dispatch <| RemoveExternalService service ) ; Button.Props <| [ Style [ MarginRight "1rem" ] ] ] [ trashIcon ]
                              div [ ClassName "media-body" ]
                                 [ str service ]

                            ]
                  )
              let externalServiceListFragments =
                  externalServiceLists
                  |> List.map (fun service ->
                        div [ ClassName "media" ]
                            [ Button.a [ Button.Option.OnClick (fun _ -> dispatch <| RemoveExternalServiceList service.ListUrl ) ; Button.Props <| [ Style [ MarginRight "1rem" ] ] ] [ trashIcon ]
                              div [ ClassName "media-body" ]
                                 [ str service.ListUrl ]

                            ]
                  )
              let feedbackMessages = renderMessages messages

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
      | ServicesTab ->
          match model.Services with
          | NotRequested -> []
          | InFlight -> [ spinnerIcon ]
          | Failed err -> [ p [] [str ("Error loading services: " + err)] ]
          | Success {PlainK8sServices = k8sServices; OrnServices = ornServices; ExternalOrnServices = externalServices; ExternalServiceLists = externalServiceLists; Messages = messages} ->
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
                            div [ ClassName "service__description"] [ str "Endpoints:" ]
                            div [ ClassName "service__info"; Style [PaddingBottom "4rem"] ]
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

              [ h3  [] [ str "OpenRiskNet services running in the VRE" ]
                div [ ClassName "row services-listing" ] ornServiceFragments
                h3  [] [ str "External services with OpenRiskNet annotation" ]
                div [ ClassName "row services-listing" ] externalServiceFragments
                h3  [] [ str "External services with OpenRiskNet annotation from dynamic service lists" ]
                div [] externalServiceListFragments
                // h3  [] [ str "Kubernetes services (debug view)" ]
                // div [] plainK8sFragments
              ]
        | DebugMessages ->
              match model.Services with
              | NotRequested -> []
              | InFlight -> [ spinnerIcon ]
              | Failed err -> [ p [] [str ("Error loading services: " + err)] ]
              | Success {PlainK8sServices = k8sServices; OrnServices = ornServices; ExternalOrnServices = externalServices; ExternalServiceLists = externalServiceLists; Messages = messages} ->
                    [ div [] (renderMessages messages) ]

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
