module Orn.Registry.Feedback

open Orn.Registry.BasicTypes
open Orn.Registry.Shared
open System.Threading

type IFeedbackAgent = IAgent<seq<TimestampedFeedback>, Feedback>

type FeedbackAgent(cancelToken: CancellationToken) =
    let capacity = 50
    let log = System.Collections.Generic.Queue<TimestampedFeedback>()

    let rec agentFunction (agent: Agent<Feedback>) =
        async {
            let! message = agent.Receive()
            lock (log) (fun _ ->
                log.Enqueue
                    ({ Feedback = message
                       Timestamp = System.DateTime.Now })
                if (log.Count > capacity) then log.Dequeue() |> ignore // throw away the oldest item))
            )

            return! agentFunction (agent)
        }

    let Agent = MailboxProcessor.Start(agentFunction, cancelToken)

    member this.Log = lock (log) (fun _ -> (log |> List.ofSeq) :> seq<TimestampedFeedback>)

    interface IFeedbackAgent with
        member this.Post(feedback: Feedback) = Agent.Post(feedback)
        member this.ReadonlyState = this.Log
