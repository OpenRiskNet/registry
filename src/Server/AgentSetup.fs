module Orn.Registry.AgentSetup

open System
open System.IO
open System.Threading

open Orn.Registry.BasicTypes

let clientPath = Path.Combine("..","Client") |> Path.GetFullPath
let port = 8085us

let getEnvironmentVariableOrDefault name defaultValue =
  match System.Environment.GetEnvironmentVariable name with
  | null -> defaultValue
  | str -> str

let k8sApiUrl = getEnvironmentVariableOrDefault "KUBERNETES_API_ENDPOINT" ""

let cancelTokenSource = new CancellationTokenSource()

let feedbackAgent = Orn.Registry.Feedback.FeedbackAgent(cancelTokenSource.Token)
let openApiAgent = OpenApiProcessing.OpenApiAgent(feedbackAgent, cancelTokenSource.Token)

let (k8sUpdateAgent : Kubernetes.UpdateAgent) = Kubernetes.UpdateAgent(feedbackAgent, openApiAgent, k8sApiUrl, cancelTokenSource.Token)


let createRefreshAgent (action : Unit -> Unit) (timeoutMs : int) : MailboxProcessor<Unit> = Agent.Start((fun agent ->
  let rec sleepRefreshLoop() =
    async {
      do! Async.Sleep(timeoutMs)
      do action()
      do! sleepRefreshLoop()
    }

  sleepRefreshLoop()
  ), cancelTokenSource.Token)

let kubernetesServicesRefreshAgent =
  createRefreshAgent
    ((k8sUpdateAgent :> IAgent<Set<Kubernetes.K8sService>, unit>).Post)
    2000
let reindexFailedServicesRefreshAgent =
  createRefreshAgent
    (fun _ -> (openApiAgent :> Orn.Registry.IAgent<Map<Shared.OpenApiUrl,OpenApiProcessing.OpenApiProcessingInformation>, OpenApiProcessing.Message>).Post(OpenApiProcessing.ReindexFailed))
    30_000

// TODO: Add giving back the indexed openrisknet servcies from openApiAgent as well
//       Add frontend second list of openrisknet services with annotation as first list
//       Annotate ChemIdConvert with corrent x-orn-@context
//       Try if it works :)
