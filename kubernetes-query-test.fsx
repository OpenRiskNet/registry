#r @"C:\Users\danyx\.nuget\packages\kubernetesclient\0.7.0-beta\lib\netstandard1.4\KubernetesClient.dll"
#r @"C:\Users\danyx\.nuget\packages\microsoft.rest.clientruntime\2.3.11\lib\netstandard1.4\Microsoft.Rest.ClientRuntime.dll"
#r @"C:\Users\danyx\.nuget\packages\system.net.http\4.3.3\lib\net46\System.Net.Http.dll"
open k8s

let config = new KubernetesClientConfiguration(Host = "http://127.0.0.1:8080")

let client = new Kubernetes(config)

let workflow = async {
  return! Async.AwaitTask(client.ListNamespacedServiceWithHttpMessagesAsync("default"))
}

let result = workflow |> Async.RunSynchronously

let names = result.Body.Items
            |> Seq.map (fun service -> service.Metadata.Name)

printfn "%s" (String.concat ", " names)
