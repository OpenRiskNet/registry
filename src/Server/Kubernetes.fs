namespace Orn.Registry.Kubernetes

open k8s
open System.Threading
open Orn.Registry.BasicTypes
open Orn.Registry.Shared
open FSharp.Interop.NullOptAble
open Orn.Registry


[<CustomComparison; CustomEquality>]
type K8sService =
    { Id: ServiceIdentifier
      Name: string
      Namespace: string
      Ports: int array
      Annotations: Map<LabelKey, string> }

    interface System.IComparable<K8sService> with
        member this.CompareTo(other: K8sService) = System.StringComparer.Ordinal.Compare(this.Id, other.Id)

    interface System.IComparable with
        member this.CompareTo(other: obj) =
            match other with
            | null -> -1
            | :? K8sService as otherService -> (this :> System.IComparable<K8sService>).CompareTo(otherService)
            | _ -> -1

    override this.Equals(other: obj) = (this :> System.IComparable).CompareTo(other) = 0
    override this.GetHashCode() = this.Id.GetHashCode()

type IKubernetesAgent = Orn.Registry.IAgent<Set<K8sService>, unit>


type UpdateAgent(feedbackAgent: Feedback.IFeedbackAgent, processingAgent: OpenApiProcessing.IOpenApiProcessingAgent, k8sApiUrl: string, cancelToken: CancellationToken) =

    let k8sconfig =
        if k8sApiUrl <> "" then KubernetesClientConfiguration(Host = k8sApiUrl)
        else KubernetesClientConfiguration.InClusterConfig()

    let client = new Kubernetes(k8sconfig)

    let mutable services = Set<K8sService> []

    let GetCurrentServices() =
        async {
            let! result = Async.AwaitTask(client.ListServiceForAllNamespacesWithHttpMessagesAsync())

            let itemsResult =
                FSharp.Interop.NullOptAble.TopLevelBuilders.option {
                    let! items = result.Body.Items

                    let services =
                        FSharp.Interop.NullOptAble.TopLevelBuilders.chooseSeq {
                            for service in items do
                                let! metadata = service.Metadata
                                let! name = metadata.Name
                                printfn "Collecting metadata for service %s" name
                                let! namespaceProperty = metadata.NamespaceProperty
                                let! spec = service.Spec
                                let! ports = spec.Ports

                                let labels =
                                    if isNull service.Metadata.Annotations then Map<LabelKey, string> []
                                    else
                                        service.Metadata.Annotations
                                        |> Seq.map (fun kvpair -> (kvpair.Key, kvpair.Value))
                                        |> Map.ofSeq

                                printfn "Metadata collected"
                                yield { Id = ServiceIdentifier name
                                        Name = name
                                        Namespace = namespaceProperty
                                        Ports =
                                            ports
                                            |> Seq.map (fun port -> port.Port)
                                            |> Seq.toArray
                                        Annotations = labels }
                        }
                    return services
                }
            match itemsResult with
            | Some items -> return (items |> Seq.toArray)
            | None -> return [||]
        }


    let rec agentFunction (processingAgent: OpenApiProcessing.IOpenApiProcessingAgent) (agent: Agent<Unit>) =
        async {
            try
                do! agent.Receive()
                let oldServices = services
                let! currentServicesSeq = GetCurrentServices()
                let currentServices = Set currentServicesSeq

                services <- currentServices
                let addedServices = currentServices - oldServices
                let removedServices = oldServices - currentServices

                for service in addedServices do
                    let openRiskNetOpenApiUrl = service.Annotations |> Map.tryFind Constants.OpenApiLabelStaticServices
                    match openRiskNetOpenApiUrl with
                    | Some rawurls ->
                        let urls = rawurls.Split('|') |> Array.map (fun url -> url.Trim())
                        printfn "OpenRiskNet definition found for service %s" service.Name
                        for url in urls do
                            processingAgent.Post
                                (OpenApiProcessing.IndexNewUrl
                                    (OpenApiUrl url, None, 60.0<FSharp.Data.UnitSystems.SI.UnitNames.second>))
                    | None -> printfn "No openrisknet definition given for %s" service.Name

                for service in removedServices do
                    let openRiskNetOpenApiUrl = service.Annotations |> Map.tryFind Constants.OpenApiLabelStaticServices
                    match openRiskNetOpenApiUrl with
                    | Some rawurls ->
                        let urls = rawurls.Split('|') |> Array.map (fun url -> url.Trim())
                        for url in urls do
                            processingAgent.Post(OpenApiProcessing.RemoveUrl(OpenApiUrl url))
                    | None -> ()
            with ex -> printfn "Excption occured in Kubernetes Agent %O" ex

            return! agentFunction processingAgent agent
        }

    let agent = MailboxProcessor.Start(agentFunction processingAgent, cancelToken)

    interface IKubernetesAgent with
        member this.Post(unit: unit) = agent.Post()
        member this.ReadonlyState = services
