namespace Orn.Registry.Kubernetes

open k8s
open System.Threading
open Orn.Registry.BasicTypes
open Orn.Registry.Shared


[<CustomComparison; CustomEquality>]
type K8sService =
  { Id : ServiceIdentifier
    Name : string
    Namespace : string
    Ports : int array
    Annotations : Map<LabelKey, string>}
    interface System.IComparable<K8sService> with
      member this.CompareTo(other : K8sService) =
        System.StringComparer.Ordinal.Compare(this.Id, other.Id)
    interface System.IComparable with
      member this.CompareTo(other : obj) =
        match other with
        | null -> -1
        | :? K8sService as otherService -> (this :> System.IComparable<K8sService>).CompareTo(otherService)
        | _ -> -1
    override this.Equals(other : obj) =
      (this :> System.IComparable).CompareTo(other) = 0
    override this.GetHashCode() =
      this.Id.GetHashCode()


type UpdateAgent(k8sApiUrl : string, cancelToken : CancellationToken) =
  let serviceAdded = new Event<SwaggerUrl>()
  let serviceRemoved = new Event<SwaggerUrl>()

  let k8sconfig =
    if k8sApiUrl <> "" then
      new KubernetesClientConfiguration(Host = k8sApiUrl)
    else
      KubernetesClientConfiguration.InClusterConfig();

  let client = new Kubernetes(k8sconfig)

  let mutable services = Set<K8sService> []

  let GetCurrentServices() =
    async {
      let! result = Async.AwaitTask(client.ListServiceForAllNamespacesWithHttpMessagesAsync())

      return
        result.Body.Items
        |> Seq.map (fun service ->
          let labels =
              if isNull service.Metadata.Annotations then
                  Map<LabelKey, string> []
              else
                  service.Metadata.Annotations
                  |> Seq.map (fun kvpair -> (kvpair.Key, kvpair.Value))
                  |> Map.ofSeq

          { Id = ServiceIdentifier service.Metadata.Name
            Name = service.Metadata.Name
            Namespace = service.Metadata.NamespaceProperty
            Ports =
              service.Spec.Ports
              |> Seq.map (fun port -> port.Port)
              |> Seq.toArray
            Annotations = labels
               }
          )
    }

  let rec AgentFunction (agent : Agent<Unit>) =
    async {
      do! agent.Receive()
      let oldServices = services
      let! currentServicesSeq = GetCurrentServices()
      let currentServices = Set currentServicesSeq

      services <- currentServices
      let addedServices = currentServices - oldServices
      let removedServices = oldServices - currentServices

      for service in addedServices do
        let openRiskNetOpenApiUrl =
          service.Annotations
          |> Map.tryFind Constants.OpenRiskNetOpenApiLabel
        match openRiskNetOpenApiUrl with
        | Some url ->
            printfn "OpenRiskNet definition found for service %s" service.Name
            serviceAdded.Trigger(SwaggerUrl url)
        | None -> printfn "No openrisknet definition given for %s" service.Name

      for service in removedServices do
        let openRiskNetOpenApiUrl =
          service.Annotations
          |> Map.tryFind Constants.OpenRiskNetOpenApiLabel
        match openRiskNetOpenApiUrl with
        | Some url -> serviceRemoved.Trigger(SwaggerUrl url)
        | None -> ()

      return! AgentFunction(agent)
    }

  let agent = MailboxProcessor.Start(AgentFunction, cancelToken)

  member this.TriggerPull() = agent.Post ()

  member this.Services = services


  [<CLIEvent>]
  member this.ServiceAdded = serviceAdded.Publish
  member this.ServiceRemoved = serviceRemoved.Publish
