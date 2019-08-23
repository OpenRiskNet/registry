open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Configuration
open Microsoft.AspNetCore.Authentication.JwtBearer
open Microsoft.IdentityModel.Protocols.OpenIdConnect
open Microsoft.IdentityModel.Tokens
open System.IdentityModel.Tokens
open System.Security

open FSharp.Control.Tasks.V2

open Orn.Registry.Shared

open Orn.Registry.Domain
open Giraffe

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us


let webApp =
    choose [
        OPTIONS >=> Successful.NO_CONTENT
        route "/api/sparql" >=> GET >=> runSparqlQueryHandler
        route "/api/services" >=> GET >=> requireUserHandler getCurrentServicesHandler
        route "/api/external-services" >=> POST >=> requireUserHandler addExternalServiceHandler
        route "/api/external-services" >=> DELETE >=> requireUserHandler removeExternalServiceHandler
        // route "/applications"
        route "/swaggerui" >=> GET >=> swaggerUiHandler
        route "/openapi-raw" >=> GET >=> rawOpenApiHandler
        route "/openapi-dereferenced" >=> GET >=> dereferencedOpenApiHandler
    ]

let buildConfig (config : IConfigurationBuilder) =
    config.SetBasePath(System.IO.Directory.GetCurrentDirectory())
          .AddYamlFile("appsettings.yaml", optional = false, reloadOnChange = true)
          .AddEnvironmentVariables()
    |> ignore

let scheme = JwtBearerDefaults.AuthenticationScheme


let configureKeycloak (config : IConfiguration) (services : IServiceCollection) =
    printfn "Keycloak domain: %s" config.["Jwt:KeycloakDomain"]
    services
        .AddAuthentication(fun config ->
            config.DefaultAuthenticateScheme <- scheme
            config.DefaultChallengeScheme <- scheme)
        .AddJwtBearer(fun options ->
            printfn "Configuring jwt bearer options"
            options.Authority <- sprintf "https://%s" config.["Jwt:KeycloakDomain"]
            options.TokenValidationParameters <-
            TokenValidationParameters(
                ValidateAudience = false
      )
        )
    |> ignore

let configureApp (app : IApplicationBuilder) =
    app.UseDefaultFiles()
        .UseStaticFiles()
        .UseForwardedHeaders()
        .UseAuthentication()
        .UseGiraffe webApp

let configureLogging (builder : ILoggingBuilder) =
    //let filter (l : LogLevel) = l >= LogLevel.Warning
    builder.AddConsole().AddDebug() |> ignore

let configureServices (services : IServiceCollection) =
    services.AddSingleton<Giraffe.Serialization.Json.IJsonSerializer>(Thoth.Json.Giraffe.ThothSerializer()) |> ignore
    services.AddGiraffe() |> ignore

    let serviceProvider = services.BuildServiceProvider()
    let config = serviceProvider.GetService<IConfiguration>()
    configureKeycloak config services

WebHost
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .ConfigureAppConfiguration(Action<_> buildConfig)
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .ConfigureLogging(configureLogging)
    .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()
