module Orn.Registry.Server

open System.IO
open System.Net

open Suave
open Suave.Operators

open Fable.Remoting.Server
open Fable.Remoting.Suave
open Orn.Registry.Kubernetes
open Orn.Registry.BasicTypes
open Orn.Registry.OpenApiProcessing

open Shared
open System.Threading
let clientPath = Path.Combine("..","Client") |> Path.GetFullPath
let port = 8085us

let getEnvironmentVariableOrDefault name defaultValue =
  match System.Environment.GetEnvironmentVariable name with
  | null -> defaultValue
  | str -> str

let k8sApiUrl = @"http://localhost:8001" //getEnvironmentVariableOrDefault "KUBERNETES_API_ENDPOINT" ""

let config =
  { defaultConfig with
      homeFolder = Some clientPath
      bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port ] }

let cancelTokenSource = new CancellationTokenSource()

let openApiAgent = OpenApiAgent(cancelTokenSource.Token)

let k8sUpdateAgent = UpdateAgent(k8sApiUrl, cancelTokenSource.Token)
k8sUpdateAgent.ServiceAdded
|> Event.add (openApiAgent.SendMessage << AddToIndex)
k8sUpdateAgent.ServiceRemoved
|> Event.add (openApiAgent.SendMessage << RemoveFromIndex)


let refreshAgent : MailboxProcessor<Unit> = Agent.Start((fun agent ->
  let rec sleepRefreshLoop() =
    async {
      do! Async.Sleep(2000)
      k8sUpdateAgent.TriggerPull()
      do! sleepRefreshLoop()
    }

  sleepRefreshLoop()
  ), cancelTokenSource.Token)

// TODO: Add giving back the indexed openrisknet servcies from openApiAgent as well
//       Add frontend second list of openrisknet services with annotation as first list
//       Annotate ChemIdConvert with corrent x-orn-@context
//       Try if it works :)

let getCurrentServices () : Async<Service list> =
  async {
    return
      k8sUpdateAgent.Services
      |> Set.toList
      |> List.map (fun k8sService -> { Name = k8sService.Name; ServiceUri = sprintf "http://%s" k8sService.Name; ServicePorts = k8sService.Ports })
  }


let init : WebPart =
  let registryProcotol =
    { getCurrentServices = getCurrentServices }
  // Create a WebPart for the given implementation of the protocol
  remoting registryProcotol {
    // define how routes are mapped
    use_route_builder Route.builder
  }

let webPart =
  choose [
    init
    Filters.path "/" >=> Files.browseFileHome "index.html"
    Files.browseHome
    RequestErrors.NOT_FOUND "Not found!"
  ]

startWebServer config webPart
