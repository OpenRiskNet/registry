module Orn.Registry.Feedback

open Orn.Registry.BasicTypes
open Orn.Registry.Shared
open System.Threading

type Feedback =
  | OpenApiDownloadFailed of SwaggerUrl
  | OpenApiParsingFailed of SwaggerUrl
  | JsonLdContextMissing of SwaggerUrl
  | JsonLdParsingError of SwaggerUrl * string

type TimestampedFeedback =
  { Feedback : Feedback
    Timestamp : System.DateTime }

type FeedbackAgent(cancelToken : CancellationToken) =
  let capacity = 100
  let log = System.Collections.Generic.Queue<TimestampedFeedback>()

  let rec AgentFunction(agent : Agent<Feedback>) =
     async {
        let! message = agent.Receive()
        log.Enqueue({Feedback = message; Timestamp = System.DateTime.Now})
        if (log.Count > capacity) then
          log.Dequeue() |> ignore // throw away the oldest item

        return! AgentFunction(agent)
     }

  let Agent = MailboxProcessor.Start(AgentFunction, cancelToken)

  member this.Post(feedback : Feedback) = Agent.Post(feedback)

  // We create a temporary list here to make sure that there is no problem during enumeration with threadsafety
  // TODO: check if this creation is ok like this
  member this.Log = (log |> List.ofSeq) :> seq<TimestampedFeedback>
