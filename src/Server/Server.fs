open System.IO
open System.Net

open Suave
open Suave.Operators

open Fable.Remoting.Server
open Fable.Remoting.Suave
open k8s

open Shared
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

let k8sconfig =
  if k8sApiUrl <> "" then
    new KubernetesClientConfiguration(Host = k8sApiUrl)
  else
    KubernetesClientConfiguration.BuildConfigFromConfigFile();


let client = new Kubernetes(k8sconfig)

let getServices() =
  async {
    let! result = Async.AwaitTask(client.ListNamespacedServiceWithHttpMessagesAsync("default"))

    return
      result.Body.Items
      |> Seq.map (fun service ->
        let labels =
            if isNull service.Metadata.Labels then
                ""
            else
                service.Metadata.Labels |> Seq.map (fun kvpair -> sprintf "%s: %s" kvpair.Key kvpair.Value) |> String.concat ", "
        let firstPort =
          service.Spec.Ports
          |> Seq.tryHead
          |> Option.map (fun port -> port.Port)
          |> Option.defaultValue 80

        { OnlineOpenApiDefinition = "http://chemidconvert:8080/swagger.json"
          Name = service.Metadata.Name
          ServiceUri = sprintf "http://%s" service.Metadata.Name
          ServicePort = firstPort }
        )
      |> Seq.toList
  }




let getCurrentServices () : Async<Service list> =
  getServices()


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
