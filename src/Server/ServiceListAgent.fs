namespace Orn.Registry.ServiceListAgent

open System
open System.Threading
open Orn.Registry.BasicTypes
open Orn.Registry.Shared
open FSharp.Interop.NullOptAble
open Orn.Registry

/// Wrapper for the standard F# agent (MailboxProcessor) that
/// supports stopping of the agent's body using the IDisposable
/// interface (the type automatically creates a cancellation token)
type AutoCancelAgent<'T> private (mbox:Agent<'T>, cts:CancellationTokenSource) =

  /// Start a new disposable agent using the specified body function
  /// (the method creates a new cancellation token for the agent)
  static member Start(f) =
    let cts = new CancellationTokenSource()
    new AutoCancelAgent<'T>(Agent<'T>.Start(f, cancellationToken = cts.Token), cts)

  member this.Post(message : 'T) =
    mbox.Post(message)

  // Disposes the agent and cancels the body
  interface IDisposable with
    member x.Dispose() =
      (mbox :> IDisposable).Dispose()
      cts.Cancel()


type IServiceListAgent =
    inherit Orn.Registry.IAgent<Set<OpenApiUrl>, Set<OpenApiUrl>>
    inherit System.IDisposable

type ServiceListAgent(feedbackAgent: Feedback.IFeedbackAgent, processingAgent: OpenApiProcessing.IOpenApiProcessingAgent) =
    let mutable currentSet = Set<OpenApiUrl> []

    let rec agentFunction (processingAgent: OpenApiProcessing.IOpenApiProcessingAgent) (agent: Agent<Set<OpenApiUrl>>) =
        async {
            let! newSet = agent.Receive()
            let oldSet = currentSet
            currentSet <- newSet

            let addedServices = newSet - oldSet
            let removedServices = oldSet - newSet

            for service in addedServices do
                processingAgent.Post
                    (OpenApiProcessing.IndexNewUrl
                        (service, None, System.Double.PositiveInfinity * 1.0<FSharp.Data.UnitSystems.SI.UnitNames.second>)) // services from a list are not updated (the generated list can use dummy querystrings if it wants to force an update)

            for service in removedServices do
                processingAgent.Post(OpenApiProcessing.RemoveUrl(service))

            return! agentFunction processingAgent agent
        }

    let agent = AutoCancelAgent.Start(agentFunction processingAgent)

    interface IServiceListAgent with
        member this.Post(newSet : Set<OpenApiUrl>) = agent.Post(newSet)
        member this.ReadonlyState = currentSet
        member this.Dispose() =
            let setAtDisposeTime = currentSet
            (agent :> IDisposable).Dispose()
            currentSet <- Set<OpenApiUrl> []
            for service in setAtDisposeTime do
                    processingAgent.Post(OpenApiProcessing.RemoveUrl(service))
