module Orn.Registry.OpenApiServicesAgent

open Orn.Registry.Shared
open Orn.Registry.BasicTypes

type CancellationToken = System.Threading.CancellationToken


type OpenRiskNetServiceInfo =
    { TripleStore: VDS.RDF.TripleStore
      OpenApiServiceInformation: OpenApiServiceInformation }

type TripleIndexingStatus =
    | InProgress
    | Indexed of OpenRiskNetServiceInfo
    | Reindexing of OpenRiskNetServiceInfo
    | Failed of string

type OpenApiRetrievalInformation =
    { OpenApiString: OpenApiRaw
      RetrievalTime: System.DateTimeOffset
      Hash: byte [] }

type OpenApiProcessingInformation =
    { Status: TripleIndexingStatus
      OpenApiRetrievalInformation: OpenApiRetrievalInformation option
      ReindexInterval: float<FSharp.Data.UnitSystems.SI.UnitNames.second>
      DereferencedOpenApi: OpenApiFixedContextEntry option }

let updateMap key newval map =
    let mutable found = false

    let newMap =
        map
        |> Map.map (fun k v ->
            if k = key then
                found <- true
                newval
            else v)
    if found then Some newMap
    else None

type ServiceMessage =
    | AddService of OpenApiUrl * OpenApiProcessingInformation
    | RemoveService of OpenApiUrl

type IOpenApiServicesAgent = Orn.Registry.IAgent<Map<OpenApiUrl, OpenApiProcessingInformation>, ServiceMessage>

type OpenRiskNetServicesAgent(cancelToken: CancellationToken) =
    let mutable serviceMap: Map<OpenApiUrl, OpenApiProcessingInformation> = Map []

    let updateServiceMap key newval =
        let updatedMapOption = updateMap key newval serviceMap
        match updatedMapOption with
        | Some updatedMap -> serviceMap <- updatedMap
        | None ->
            let added = Map.add key newval serviceMap
            serviceMap <- added


    let rec agentFunction (agent: Agent<ServiceMessage>) =
        async {
            let! message = agent.Receive()
            match message with
            | AddService(url, information) -> updateServiceMap url information
            | RemoveService url -> serviceMap <- serviceMap |> Map.remove url
            do! agentFunction (agent)
        }

    let agent = Agent.Start(agentFunction, cancelToken)

    interface IOpenApiServicesAgent with
        member this.Post(message: ServiceMessage) = agent.Post(message)
        member this.ReadonlyState = serviceMap
