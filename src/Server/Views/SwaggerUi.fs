module Orn.Registry.Views.SwaggerUi

open Giraffe.GiraffeViewEngine

let swaggerUi openapiContent =
    let pageTitle = "SwaggerUi"

    let escapedOpenApiContent =
        System.Web.HttpUtility.JavaScriptStringEncode openapiContent
        |> fun content -> content.Replace("\n", "")

    let swaggerUiBlock = """
var ui = SwaggerUIBundle({
spec: openapi,
dom_id: '#swagger-ui',
presets: [
  SwaggerUIBundle.presets.apis,
  SwaggerUIBundle.SwaggerUIStandalonePreset
]
});"""

    let fullContent =
        System.Text.StringBuilder()
            .Append("var openapijson = \"")
            .Append(escapedOpenApiContent)
            .AppendLine("\";")
            .AppendLine("var openapi = JSON.parse(openapijson);")
            .AppendLine(swaggerUiBlock)
            .ToString()

    let content =
        [ div [ _style "max-width: 100%; background-color: rgba(97,175,254,.1); padding: 0.4em; font-family: sans-serif;" ] [ str "Please note that since SwaggerUI runs in your browser (and not inside the cluster), the 'try it out' functionality will not work"]
          div [_id "swagger-ui"] []
          script [_src @"https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/3.20.2/swagger-ui-bundle.js" ] []
          script [ ]
            [ rawText  fullContent
          ]
        ]
    html [] [ head [] [ title [] [ str pageTitle ]
                        link [ _rel "stylesheet"
                               _href @"https://cdnjs.cloudflare.com/ajax/libs/swagger-ui/3.20.2/swagger-ui.css" ] ]
              body [] content ]
