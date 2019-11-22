module Orn.Registry.AgentSetup

open System
open System.IO
open System.Threading
open FSharp.Data.UnitSystems.SI.UnitNames
open OpenApiProcessing
open Orn.Registry.BasicTypes

let clientPath = Path.Combine("..", "Client") |> Path.GetFullPath
let port = 8085us

let getEnvironmentVariableOrDefault name defaultValue =
    match System.Environment.GetEnvironmentVariable name with
    | null -> defaultValue
    | str -> str

let k8sApiUrl = getEnvironmentVariableOrDefault "KUBERNETES_API_ENDPOINT" ""

let cancelTokenSource = new CancellationTokenSource()

let private feedbackAgentImpl = Orn.Registry.Feedback.FeedbackAgent(cancelTokenSource.Token)
let feedbackAgent = feedbackAgentImpl :> Feedback.IFeedbackAgent
let private openApiServicesAgentImpl = OpenApiServicesAgent.OpenRiskNetServicesAgent(cancelTokenSource.Token)
let openApiServicesAgent = openApiServicesAgentImpl :> OpenApiServicesAgent.IOpenApiServicesAgent
let private processingAgents =
    seq { 1 .. 19 } // create 19 processing agents - prime numbers are probably better since we do simple round robin }
    |> Seq.map
        (fun _ ->
        OpenApiProcessing.OpenApiProcessingAgent(feedbackAgent, openApiServicesAgent, cancelTokenSource.Token) :> IOpenApiProcessingAgent)

let private openApiProcessingAgentImpl: AgentLoadBalancing.AgentLoadBalancingAgent<bool, ProcessingMessage, IOpenApiProcessingAgent> =
    AgentLoadBalancing.AgentLoadBalancingAgent(processingAgents, (fun _ _ -> true), cancelTokenSource.Token)

let openApiProcessingAgent = openApiProcessingAgentImpl :> IOpenApiProcessingAgent

let k8sUpdateAgent: Result<Kubernetes.IKubernetesAgent, string> =
    try
        Ok <| (Kubernetes.UpdateAgent(feedbackAgent, openApiProcessingAgent, k8sApiUrl, cancelTokenSource.Token) :> Kubernetes.IKubernetesAgent)
    with ex -> Error (ex.Message)

let private listManagementAgentImpl : Orn.Registry.ListManagementAgent.ListManagementAgent = Orn.Registry.ListManagementAgent.ListManagementAgent(feedbackAgent, openApiProcessingAgent, cancelTokenSource.Token)

let listManagementAgent = listManagementAgentImpl :> Orn.Registry.ListManagementAgent.IListManagementAgent

let createRefreshAgent (action: Unit -> Unit) (timeout: float<second>): MailboxProcessor<Unit> =
    Agent.Start
        ((fun agent ->
         let rec sleepRefreshLoop() =
             async {
                 do! Async.Sleep(int (timeout * millisecondsPerSecond))
                 do action()
                 do! sleepRefreshLoop()
             }
         sleepRefreshLoop()), cancelTokenSource.Token)

let private kubernetesServicesRefreshAgent =
    k8sUpdateAgent |> Result.map (fun k8sAgent -> createRefreshAgent k8sAgent.Post 2.0<second>)

let private listsRefreshAgent = createRefreshAgent (fun _ -> listManagementAgent.Post Orn.Registry.ListManagementAgent.RefreshLists ) 360.0<second>

let private reindexFailedServicesRefreshAgent =
    createRefreshAgent (fun _ ->
        let mutable reindexingCount = 0
        let services = openApiServicesAgent.ReadonlyState
        services
        |> Map.iter (fun key value ->
            match value with
            | { Status = OpenApiServicesAgent.Indexed _; OpenApiRetrievalInformation = Some retrievalInfo;
                ReindexInterval = reindexInterval } when (retrievalInfo.RetrievalTime
                                                          + (secondsToTimeSpan reindexInterval)) < System.DateTimeOffset.UtcNow ->
                reindexingCount <- reindexingCount + 1
                openApiProcessingAgent.Post(IndexNewUrl(key, Some value, reindexInterval))
            | { Status = OpenApiServicesAgent.Failed _; ReindexInterval = reindexInterval } ->
                reindexingCount <- reindexingCount + 1
                openApiProcessingAgent.Post(IndexNewUrl(key, Some value, reindexInterval))
            | _ -> ())) 60.0<second>

listManagementAgent.Post(ListManagementAgent.AddNewList "https://raw.githubusercontent.com/OpenRiskNet/registry/master/defaultservices.json")
