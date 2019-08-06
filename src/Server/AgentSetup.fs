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
let openApiServicesAgent = OpenApiServicesAgent.OpenRiskNetServicesAgent(cancelTokenSource.Token)
let processingAgents =
  seq {1..19}
  |> Seq.map (fun _ -> OpenApiProcessing.OpenApiProcessingAgent(feedbackAgent, openApiServicesAgent, cancelTokenSource.Token) :> OpenApiProcessing.IOpenApiProcessingAgent)
let (openApiProcessingAgent : AgentLoadBalancing.AgentLoadBalancingAgent<bool, OpenApiProcessing.ProcessingMessage, OpenApiProcessing.IOpenApiProcessingAgent>) =
  AgentLoadBalancing.AgentLoadBalancingAgent(processingAgents, (fun _ _ -> true), cancelTokenSource.Token)


let (k8sUpdateAgent : Kubernetes.UpdateAgent) = Kubernetes.UpdateAgent(feedbackAgent, openApiProcessingAgent, k8sApiUrl, cancelTokenSource.Token)


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
    ((k8sUpdateAgent :> Kubernetes.IKubernetesAgent).Post)
    2000
// TODO: reimplement reindexing agent
// let reindexFailedServicesRefreshAgent =
//   createRefreshAgent
//     (fun _ -> (openApiAgent :> OpenApiProcessing.IOpenApiProcessingAgent).Post(OpenApiProcessing.ReindexFailed))
//     30_000
