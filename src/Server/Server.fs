module Orn.Registry.Server

open System.IO
open System.Net

open Suave
open Suave.Operators

open Fable.Remoting.Server
open Fable.Remoting.Suave
open Orn.Registry.BasicTypes

open Orn.Registry
open System.Threading
let clientPath = Path.Combine("..","Client") |> Path.GetFullPath
let port = 8085us

let getEnvironmentVariableOrDefault name defaultValue =
  match System.Environment.GetEnvironmentVariable name with
  | null -> defaultValue
  | str -> str

let k8sApiUrl = getEnvironmentVariableOrDefault "KUBERNETES_API_ENDPOINT" ""

let config =
  { defaultConfig with
      homeFolder = Some clientPath
      bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port ] }

let cancelTokenSource = new CancellationTokenSource()


let openApiAgent = OpenApiProcessing.OpenApiAgent(cancelTokenSource.Token)

let k8sUpdateAgent = Kubernetes.UpdateAgent(k8sApiUrl, cancelTokenSource.Token)
k8sUpdateAgent.ServiceAdded
|> Event.add (openApiAgent.SendMessage << OpenApiProcessing.AddToIndex)
k8sUpdateAgent.ServiceRemoved
|> Event.add (openApiAgent.SendMessage << OpenApiProcessing.RemoveFromIndex)


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

let getCurrentServices () : Async<Shared.ActiveServices> =
  async {
    let k8sServices =
      k8sUpdateAgent.Services

    let ornServices =
      openApiAgent.ServiceMap

    let k8sServiceWithOptionalOrnService =
      k8sServices
      |> Seq.map ((fun service -> service.Annotations |> Map.tryFind Shared.Constants.OpenRiskNetOpenApiLabel )
               >> (fun swaggerurlOption -> swaggerurlOption |> Option.bind (fun swaggerurl -> ornServices |> Map.tryFind (Shared.SwaggerUrl swaggerurl) ) )
               >> (fun indexStatusOption ->
                    match indexStatusOption with
                    | None -> None
                    | Some OpenApiProcessing.InProgress -> None
                    | Some (OpenApiProcessing.Failed _) -> None
                    | Some (OpenApiProcessing.Indexed ornInfo) -> Some ornInfo))
      |> Seq.zip (k8sServices)

    let k8sServicesOnly, ornServices =
      k8sServiceWithOptionalOrnService
      |> Seq.fold
          (fun (servicesOnly, ornservices) currentServicePair ->
             match currentServicePair with
             | (service, Some ornService) -> (servicesOnly, ( service, ornService) :: ornservices)
             | (service, None) -> (service :: servicesOnly, ornservices)
          ) ([], [])

    let internalK8sServiceToClientFormat (k8sService : Kubernetes.K8sService) =
      { Shared.Name = k8sService.Name
        Shared.ServicePorts = k8sService.Ports}

    let internalK8sServicesToClientFormat (internalK8sServices : Kubernetes.K8sService list) =
      internalK8sServices
      |> Seq.map internalK8sServiceToClientFormat

    let clientFormatK8sOnlyServices =
      internalK8sServicesToClientFormat k8sServicesOnly
      |> List.ofSeq

    let clientFormatOrnServices =
      ornServices
      |> Seq.map (fun (k8sService, openApiService) -> { Shared.K8sService = internalK8sServiceToClientFormat k8sService
                                                        Shared.OpenApiServiceInformation = openApiService.OpenApiServiceInformation })
      |> List.ofSeq

    return
      { Shared.PlainK8sServices = clientFormatK8sOnlyServices
        Shared.OrnServices = clientFormatOrnServices  }

  }


let init : WebPart =
  let registryProcotol =
    { Shared.getCurrentServices = getCurrentServices }
  // Create a WebPart for the given implementation of the protocol
  remoting registryProcotol {
    // define how routes are mapped
    use_route_builder Shared.Route.builder
  }

let webPart =
  choose [
    init
    Filters.path "/" >=> Files.browseFileHome "index.html"
    Files.browseHome
    RequestErrors.NOT_FOUND "Not found!"
  ]

startWebServer config webPart
