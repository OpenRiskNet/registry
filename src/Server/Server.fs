open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

open FSharp.Control.Tasks.V2
open Giraffe
open Orn.Registry.Shared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Orn.Registry.Domain

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let buildRegistryProtocol (context : Http.HttpContext) =
    let logger = context.GetLogger()
    { getCurrentServices = getCurrentServices logger }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromContext buildRegistryProtocol
    |> Remoting.buildHttpHandler


let configureApp (app : IApplicationBuilder) =
    app.UseDefaultFiles()
       .UseStaticFiles()
       .UseGiraffe webApp

let configureLogging (builder : ILoggingBuilder) =
    //let filter (l : LogLevel) = l >= LogLevel.Warning
    builder.AddConsole().AddDebug() |> ignore

let configureServices (services : IServiceCollection) =
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
