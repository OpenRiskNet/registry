open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

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
        route "/api/services" >=> GET >=> getCurrentServicesHandler
        // route "/applications"
        route "/openapi" >=> GET >=> swaggerUiHandler
    ]


let configureApp (app : IApplicationBuilder) =
    app.UseDefaultFiles()
       .UseStaticFiles()
       .UseGiraffe webApp

let configureLogging (builder : ILoggingBuilder) =
    //let filter (l : LogLevel) = l >= LogLevel.Warning
    builder.AddConsole().AddDebug() |> ignore

let configureServices (services : IServiceCollection) =
    services.AddSingleton<Giraffe.Serialization.Json.IJsonSerializer>(Thoth.Json.Giraffe.ThothSerializer()) |> ignore
    services.AddGiraffe() |> ignore

WebHost
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .ConfigureLogging(configureLogging)
    .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()
