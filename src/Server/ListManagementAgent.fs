module Orn.Registry.ListManagementAgent

open Orn.Registry.BasicTypes
open DouglasConnect.Http
open Cvdm.ErrorHandling
open Orn.Registry.Shared
open Orn.Registry.OpenApiProcessing
open System.Threading

type ListProcessingMessage =
    | AddNewList of string
    | RemoveList of string
    | RefreshLists

type IListManagementAgent = Orn.Registry.IAgent<bool, ListProcessingMessage> // The state should be unit but there is a weird compiler error if I return unit

type ListManagementAgent(feedbackAgent: Feedback.IFeedbackAgent, openApiAgent: IOpenApiProcessingAgent, cancelToken: CancellationToken) =
    let mutable serviceListAgents = Map<string, Result<Orn.Registry.ServiceListAgent.IServiceListAgent, string>> []

    let retrieveOpenApiSet url =
        asyncResult {
            try
                printfn "Downloading service list for %s" url
                let headers =
                  [ FSharp.Data.HttpRequestHeaders.Accept FSharp.Data.HttpContentTypes.Json ]
                let! text = SafeAsyncHttp.AsyncHttpTextResult
                                       (url, timeout = System.TimeSpan.FromSeconds(20.0),
                                        headers = headers)
                                   |> AsyncResult.mapError (fun err -> err.ToString())
                                   |> AsyncResult.teeError
                                       (fun _ ->
                                       feedbackAgent.Post
                                           (Orn.Registry.Shared.ListDownloadFailed
                                               (url)))
                let! rawList =
                    text
                    |> Thoth.Json.Net.Decode.fromString (Thoth.Json.Net.Decode.array Thoth.Json.Net.Decode.string)
                let openApiSet = rawList |> Array.map OpenApiUrl |> Set.ofArray
                return openApiSet
            with
                | :? System.Net.WebException ->
                    printfn "Timeout occured in OpenApi processing agent when processing: %s" url
                    return! Error (sprintf "Timeout occured in OpenApi processing agent when processing: %s" url)
                | ex ->
                    printfn "Exception occured in OpenApi processing agent: %O" ex
                    return! Error (sprintf "Exception occured in OpenApi processing agent: %O" ex)
        }

    let createAndRunServiceListAgent openApiSet =
        let agent = new Orn.Registry.ServiceListAgent.ServiceListAgent(feedbackAgent, openApiAgent) :> Orn.Registry.ServiceListAgent.IServiceListAgent
        agent.Post(openApiSet)
        Ok agent

    let rec agentFunction (feedbackAgent: Feedback.IFeedbackAgent) (openApiAgent: IOpenApiProcessingAgent)
            (agent: Agent<ListProcessingMessage>) =
        async {

            let! message = agent.Receive()
            match message with
            | AddNewList(url) ->
                    if not (Map.containsKey url serviceListAgents) then
                        let! openApiSetResult = retrieveOpenApiSet url
                        let serviceEntry =
                            match openApiSetResult with
                            | Ok openApiSet ->
                                createAndRunServiceListAgent openApiSet
                            | Error err ->
                                Error err
                        serviceListAgents <- Map.add url serviceEntry serviceListAgents
                        ()
                    else
                        ()
            | RemoveList url ->
                let agent = Map.tryFind url serviceListAgents
                match agent with
                | Some (Ok listAgent) ->
                    listAgent.Dispose()
                    serviceListAgents <- Map.remove url serviceListAgents
                | Some (Error _) ->
                    serviceListAgents <- Map.remove url serviceListAgents
                | None ->
                    ()
            | RefreshLists ->
                let urlsAndResults =
                    Map.toArray serviceListAgents
                let! refreshedServiceLists =
                    urlsAndResults
                    |> Array.map
                        (fun (url, previousResult) ->
                            async {
                                let! newResult = retrieveOpenApiSet url
                                return
                                    match previousResult, newResult with
                                    | Ok agent, Ok currentList ->
                                        agent.Post(currentList)
                                        url, Ok agent
                                    | Error _, Ok currentList ->
                                        url, (createAndRunServiceListAgent currentList)
                                    | Ok agent, Error _ ->
                                        url, Ok agent
                                    | Error _, Error err ->
                                        url, Error err
                            }
                                 )
                    |> Async.Parallel
                serviceListAgents <- Map.ofArray refreshedServiceLists

            do! agentFunction feedbackAgent openApiAgent agent
        }

    let agent = Agent.Start(agentFunction feedbackAgent openApiAgent, cancelToken)

    interface IListManagementAgent with
        member this.Post(message: ListProcessingMessage) = agent.Post(message)
        member this.ReadonlyState =
            true //TODO: if there is a weird error that no services sho up then the reason could be that this is a closure which I hope it is not
