module Orn.Registry.AgentLoadBalancing

open Orn.Registry

open Orn.Registry.BasicTypes
type CancellationToken = System.Threading.CancellationToken

type IAgentLoadBalancingAgent<'state, 'message> = IAgent<'state, 'message>


type AgentLoadBalancingAgent<'state, 'message, 'agent when 'agent :> IAgent<'state, 'message>>
    (agents : IAgent<'state, 'message> seq,
    // combine two state instances into one - required to be able to merge the
    // results of all the agents into one result value without changing the state
    // type to a list so that this agent can be used as a drop-in
    // In Haskell this would be Semigroup.mappend
     mergeStatesFn : ('state -> 'state -> 'state),
     cancelToken : CancellationToken) =
    let agents = Array.ofSeq agents

    let mutable nextAgentIndex = 0

    let rec agentFunction (agent : Agent<'message>) =
        async {
            let! msg = agent.Receive()
            agents.[nextAgentIndex].Post(msg)
            nextAgentIndex <- (nextAgentIndex + 1) % (agents.Length)
            do! agentFunction agent
        }

    let agent = MailboxProcessor.Start(agentFunction, cancelToken)

    interface IAgentLoadBalancingAgent<'state, 'message> with
        member this.ReadonlyState = agents |> Seq.map (fun agent -> agent.ReadonlyState) |> Seq.reduce mergeStatesFn
        member this.Post(msg : 'message) = agent.Post(msg)
