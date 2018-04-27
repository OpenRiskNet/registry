namespace Orn.Registry.Kubernetes

open k8s
open System.Threading
open Orn.Registry.BasicTypes



[<CustomComparison; CustomEquality>]
type K8sService =
  { Id : ServiceIdentifier
    Name : string
    Namespace : string
    Ports : int array
    Labels : Map<LabelKey, string>}
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
  let k8sconfig =
    if k8sApiUrl <> "" then
      new KubernetesClientConfiguration(Host = k8sApiUrl)
    else
      KubernetesClientConfiguration.InClusterConfig();

  let client = new Kubernetes(k8sconfig)

  let mutable services = Set<K8sService> []

  let GetCurrentServices() =
    async {
      let! result = Async.AwaitTask(client.ListNamespacedServiceWithHttpMessagesAsync("default"))

      return
        result.Body.Items
        |> Seq.map (fun service ->
          let labels =
              if isNull service.Metadata.Labels then
                  Map<LabelKey, string> []
              else
                  service.Metadata.Labels
                  |> Seq.map (fun kvpair -> (kvpair.Key, kvpair.Value))
                  |> Map.ofSeq

          { Id = ServiceIdentifier service.Metadata.Name
            Name = service.Metadata.Name
            Namespace = service.Metadata.NamespaceProperty
            Ports =
              service.Spec.Ports
              |> Seq.map (fun port -> port.Port)
              |> Seq.toArray
            Labels = labels
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

      // TODO: notify swagger indexing agent of added/removed services


      return! AgentFunction(agent)
    }

  let agent = MailboxProcessor.Start(AgentFunction, cancelToken)

  member this.Agent = agent

  member this.Services = services
