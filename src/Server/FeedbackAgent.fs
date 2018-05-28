module Orn.Registry.Feedback

open Orn.Registry.BasicTypes
open Orn.Registry.Shared
open System.Threading

type FeedbackAgent(cancelToken : CancellationToken) =
  let capacity = 50
  let log = System.Collections.Generic.Queue<TimestampedFeedback>()

  let rec AgentFunction(agent : Agent<Feedback>) =
     async {
        let! message = agent.Receive()
        lock (log) (fun _ ->
          log.Enqueue({Feedback = message; Timestamp = System.DateTime.Now})
          if (log.Count > capacity) then
            log.Dequeue() |> ignore // throw away the oldest item
          )

        return! AgentFunction(agent)
     }

  let Agent = MailboxProcessor.Start(AgentFunction, cancelToken)

  member this.Post(feedback : Feedback) = Agent.Post(feedback)

  member this.Log =
    lock (log) (fun _ ->
      (log |> List.ofSeq) :> seq<TimestampedFeedback>
    )
