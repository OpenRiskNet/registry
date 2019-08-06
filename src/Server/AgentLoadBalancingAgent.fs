module Orn.Registry.AgentLoadBalancing

open Orn.Registry

open Orn.Registry.BasicTypes
type CancellationToken = System.Threading.CancellationToken

type IAgentLoadBalancingAgent<'state, 'message> = IAgent<'state seq, 'message>

type AgentLoadBalancingAgent<'state, 'message, 'agent when 'agent :> IAgent<'state, 'message>>(agents : IAgent<'state, 'message> seq, cancelToken : CancellationToken) =
    let Agents = Array.ofSeq agents

    let mutable nextAgentIndex = 0

    let rec agentFunction (agent : Agent<'message>) =
        async {
            let! msg = agent.Receive()
            Agents.[nextAgentIndex].Post(msg)
            nextAgentIndex <- (nextAgentIndex + 1) % (Agents.Length)
            do! agentFunction agent
        }

    let agent = MailboxProcessor.Start(agentFunction, cancelToken)

    interface IAgentLoadBalancingAgent<'state, 'message> with
        member this.ReadonlyState = Agents |> Seq.map (fun agent -> agent.ReadonlyState)
        member this.Post(msg : 'message) = agent.Post(msg)
