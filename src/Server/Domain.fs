module Orn.Registry.Domain

open Orn.Registry.BasicTypes
open Microsoft.Extensions.Logging

open Orn.Registry
open Orn.Registry.AgentSetup
open Orn.Registry.JsonLdParsing
open Orn.Registry.Shared

// TODO: move these two into agents that make sure that this bookkkeping is threadsafe
let mutable ExternalServices = Set.empty<string>

let getCurrentServices (isDevMode : bool) (logger: ILogger) email: Async<Shared.ActiveServices> =
    async {

        logger.LogInformation "Entered getCurrentServices"

        let k8sServices =
            match k8sUpdateAgent, isDevMode with
            | Ok agent, _ -> agent.ReadonlyState
            | Error _, true -> Set.empty
            | Error err, false -> failwith "Kubernetes Agent could not be initialized"

        let ornServices = openApiServicesAgent.ReadonlyState

        let externalServices = ExternalServices

        let externalServiceWithOptionalOrnService =
            ExternalServices
            |> Seq.choose
                ((fun openApiUrl -> ornServices |> Map.tryFind (Shared.OpenApiUrl openApiUrl))
                 >> (fun indexStatusOption ->
                 match indexStatusOption with
                 | None -> None
                 | Some { Status = OpenApiServicesAgent.InProgress } -> None
                 | Some { Status = (OpenApiServicesAgent.Failed _) } -> None
                 | Some { Status = (OpenApiServicesAgent.Reindexing ornInfo) }
                 | Some { Status = (OpenApiServicesAgent.Indexed ornInfo) } ->
                     Some { OpenApiServiceInformation = ornInfo.OpenApiServiceInformation }))
            |> Seq.toList


        let externalServiceLists =
            listManagementAgent.ReadonlyState
            |> Map.toList
            |> List.map
                ((fun (listUrl, listResult) ->
                    let serviceList =
                        match listResult with
                        | Ok set ->
                            set
                            |> Set.toList
                            |> List.choose (fun service ->
                                let maybeProcessingInfo = ornServices |> Map.tryFind service
                                match maybeProcessingInfo with
                                | None ->
                                    None
                                | Some { Status = OpenApiServicesAgent.InProgress } ->
                                    None
                                | Some { Status = (OpenApiServicesAgent.Failed _) } ->
                                    None
                                | Some { Status = (OpenApiServicesAgent.Reindexing ornInfo) }
                                | Some { Status = (OpenApiServicesAgent.Indexed ornInfo) } ->
                                    Some ornInfo.OpenApiServiceInformation
                            )
                        | Error _ ->
                            []
                    { ListUrl = listUrl
                      Services = serviceList }
                ))




        let k8sServiceWithOptionalOrnService =
            k8sServices
            |> Seq.map
                ((fun service -> service.Annotations |> Map.tryFind Shared.Constants.OpenApiLabelStaticServices)
                 >> (fun openApiUrlOption ->
                 openApiUrlOption
                 |> Option.bind (fun openApiUrl -> ornServices |> Map.tryFind (Shared.OpenApiUrl openApiUrl)))
                 >> (fun indexStatusOption ->
                 match indexStatusOption with
                 | None -> None
                 | Some { Status = OpenApiServicesAgent.InProgress } -> None
                 | Some { Status = (OpenApiServicesAgent.Failed _) } -> None
                 | Some { Status = (OpenApiServicesAgent.Reindexing ornInfo) }
                 | Some { Status = (OpenApiServicesAgent.Indexed ornInfo) } -> Some ornInfo))
            |> Seq.zip (k8sServices)

        let k8sServicesOnly, ornServices =
            k8sServiceWithOptionalOrnService
            |> Seq.fold (fun (servicesOnly, ornservices) currentServicePair ->
                match currentServicePair with
                | (service, Some ornService) -> (servicesOnly, (service, ornService) :: ornservices)
                | (service, None) -> (service :: servicesOnly, ornservices)) ([], [])

        let internalK8sServiceToClientFormat (k8sService: Kubernetes.K8sService) =
            { Shared.Name = k8sService.Name
              Shared.ServicePorts = k8sService.Ports }

        let internalK8sServicesToClientFormat (internalK8sServices: Kubernetes.K8sService list) =
            internalK8sServices |> Seq.map internalK8sServiceToClientFormat

        let clientFormatK8sOnlyServices = internalK8sServicesToClientFormat k8sServicesOnly |> List.ofSeq

        let clientFormatOrnServices =
            ornServices
            |> Seq.map (fun (k8sService, openApiService) ->
                { Shared.K8sService = internalK8sServiceToClientFormat k8sService
                  Shared.OpenApiServiceInformation = openApiService.OpenApiServiceInformation })
            |> List.ofSeq

        return { Shared.PlainK8sServices = clientFormatK8sOnlyServices
                 Shared.OrnServices = clientFormatOrnServices
                 Shared.ExternalOrnServices = externalServiceWithOptionalOrnService
                 Shared.ExternalServices = Set.toList externalServices
                 Shared.ExternalServiceLists = externalServiceLists
                 Shared.Messages = feedbackAgent.ReadonlyState |> Seq.toList }
    }




let runSparqlQuery (logger: ILogger) (maybeService: OpenApiUrl option) (query: string): Result<SparqlResultsForServices, string> =
    result {
        let timer = System.Diagnostics.Stopwatch()
        timer.Start()
        let! parsedQuery = createSparqlQuery query
        let timeToParsed = timer.Elapsed
        logger.LogInformation(sprintf "Parsing sparql query took %f milliseconds" timeToParsed.TotalMilliseconds)

        let ornServices = openApiServicesAgent.ReadonlyState

        let openApisAndTripleStores =
            ornServices
            |> Map.fold
                (fun state key value ->
                match value.Status with
                | OpenApiServicesAgent.InProgress -> state
                | OpenApiServicesAgent.Failed _ -> state
                | OpenApiServicesAgent.Reindexing serviceInfo
                | OpenApiServicesAgent.Indexed serviceInfo ->
                    (key, serviceInfo.OpenApiServiceInformation.Name, serviceInfo.TripleStore) :: state) []

        let sources =
            match maybeService with
            | None -> openApisAndTripleStores
            | Some service -> openApisAndTripleStores |> List.filter (fun (openapiUrl, _, _) -> openapiUrl = service)

        let results =
            sources
            |> List.toArray
            |> Array.Parallel.map (fun (openapiUrl, name, triples) ->
                let result = runQuery triples parsedQuery
                openapiUrl, name, result)

        let resultsWithEmptyListForErrors =
            results
            |> Array.map (fun (openapiUrl, name, result) ->
                match result with
                | Ok resultValue ->
                    { ServiceName = name
                      OpenApiUrl = openapiUrl
                      Result = resultValue }
                | Error error ->
                    logger.LogError(sprintf "Error when running query %s against url %O: %s" query openapiUrl error)
                    { ServiceName = name
                      OpenApiUrl = openapiUrl
                      Result = NoResult })

        let timeToQueried = timer.Elapsed
        logger.LogInformation
            (sprintf "Executing sparql query against %d services took %f milliseconds" (sources.Length)
                 (timeToQueried - timeToParsed).TotalMilliseconds)
        timer.Stop()

        return resultsWithEmptyListForErrors
    }
