module Orn.Registry.ListRefreshAgent

open Orn.Registry.BasicTypes
open DouglasConnect.Http
open Cvdm.ErrorHandling
open Orn.Registry.Shared

type ListProcessingMessage =
    | AddNewList of string * float<FSharp.Data.UnitSystems.SI.UnitNames.second>
    | RemoveList of string
    | RefreshLists

type IListRefreshAgent = Orn.Registry.IAgent<bool, ListProcessingMessage> // The state should be unit but there is a weird compiler error if I return unit

type OpenApiProcessingAgent(feedbackAgent: Feedback.IFeedbackAgent, openApiAgent: IOpenApiProcessingAgent, cancelToken: CancellationToken) =
    let mutable serviceListAgents = Map<string, Result<Orn.Registry.ServiceListAgent.IServiceListAgent, string>> []

    let private retrieveOpenApiSet url =
        asyncResult {
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
                                       (Orn.Registry.Shared.OpenApiDownloadFailed
                                           (openApiUrl)))
            let! rawList =
                text
                |> Thoth.Json.Net.Decode.fromString (Thoth.Json.Net.Decode.array Thoth.Json.Net.Decode.string)
            let openApiSet = rawList |> List.map OpenApiUrl |> Set.ofList
            return openApiSet
        }

    let rec agentFunction (feedbackAgent: Feedback.IFeedbackAgent) (openApiAgent: IOpenApiProcessingAgent)
            (agent: Agent<ProcessingMessage>) =
        async {

            let! message = agent.Receive()
            match message with
            | IndexNewUrl(url, reindexInterval) ->
                try
                    let! result =
                      asyncResult {
                            if not (Map.containsKey url serviceListAgents) then
                                let! openApiSet = retrieveOpenApiSet url
                                let agent =
                                    Orn.Registry.ServiceListAgent.ServiceListAgent(feedbackAgent, openApiAgent) :> Orn.Registry.ServiceListAgent.IServiceListAgent
                                agent.Post(openApiList)
                                serviceListAgents <- Map.add url (Ok agent) serviceListAgents
                                ()
                            else
                                ()
                      }



                    match result with
                    | Ok(serviceInformation, tripleStore) ->
                        printfn "Loading json-ld into triple store worked for service %s - memory use is: %.2f MB" url (getUsedMemoryInMb())
                        servicesAgent.Post
                            (AddService
                                (openApiUrl,
                                 { processingInfo with
                                       Status =
                                           Indexed
                                               { TripleStore = tripleStore
                                                 OpenApiServiceInformation = serviceInformation } }))
                    | Error msg ->
                        printfn "Loading json-ld into triple store FAILED for service %s" url
                        servicesAgent.Post(AddService(openApiUrl, { processingInfo with Status = Failed msg }))
                with

                | :? System.Net.WebException ->
                    printfn "Timeout occured in OpenApi processing agent when processing: %s" url
                    servicesAgent.Post
                        (AddService
                            (openApiUrl,
                             ({ Status = Failed "Timeout while trying to download swagger definition"
                                OpenApiRetrievalInformation = None
                                DereferencedOpenApi = None
                                ReindexInterval = reindexInterval })))

                | ex ->
                    feedbackAgent.Post(JsonLdParsingError(OpenApiUrl url, ex.ToString()))
                    printfn "Exception occured in OpenApi processing agent: %O" ex

            | RemoveUrl url -> servicesAgent.Post(RemoveService url)

            do! agentFunction feedbackAgent servicesAgent agent
        }

    let agent = Agent.Start(agentFunction feedbackAgent servicesAgent, cancelToken)

    interface IOpenApiProcessingAgent with
        member this.Post(message: ProcessingMessage) = agent.Post(message)
        member this.ReadonlyState =
            true //TODO: if there is a weird error that no services sho up then the reason could be that this is a closure which I hope it is not
