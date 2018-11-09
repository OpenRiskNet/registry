module Orn.Registry.Domain

open Orn.Registry.BasicTypes

open Orn.Registry
open Orn.Registry.AgentSetup

let getCurrentServices () : Async<Shared.ActiveServices> =
  async {
    let k8sServices =
      k8sUpdateAgent.Services

    let ornServices =
      openApiAgent.ServiceMap

    let k8sServiceWithOptionalOrnService =
      k8sServices
      |> Seq.map ((fun service -> service.Annotations |> Map.tryFind Shared.Constants.OpenApiLabelStaticServices )
               >> (fun swaggerurlOption -> swaggerurlOption |> Option.bind (fun swaggerurl -> ornServices |> Map.tryFind (Shared.SwaggerUrl swaggerurl) ) )
               >> (fun indexStatusOption ->
                    match indexStatusOption with
                    | None -> None
                    | Some OpenApiProcessing.InProgress -> None
                    | Some (OpenApiProcessing.Failed _) -> None
                    | Some (OpenApiProcessing.Indexed ornInfo) -> Some ornInfo))
      |> Seq.zip (k8sServices)

    let k8sServicesOnly, ornServices =
      k8sServiceWithOptionalOrnService
      |> Seq.fold
          (fun (servicesOnly, ornservices) currentServicePair ->
             match currentServicePair with
             | (service, Some ornService) -> (servicesOnly, ( service, ornService) :: ornservices)
             | (service, None) -> (service :: servicesOnly, ornservices)
          ) ([], [])

    let internalK8sServiceToClientFormat (k8sService : Kubernetes.K8sService) =
      { Shared.Name = k8sService.Name
        Shared.ServicePorts = k8sService.Ports}

    let internalK8sServicesToClientFormat (internalK8sServices : Kubernetes.K8sService list) =
      internalK8sServices
      |> Seq.map internalK8sServiceToClientFormat

    let clientFormatK8sOnlyServices =
      internalK8sServicesToClientFormat k8sServicesOnly
      |> List.ofSeq

    let clientFormatOrnServices =
      ornServices
      |> Seq.map (fun (k8sService, openApiService) -> { Shared.K8sService = internalK8sServiceToClientFormat k8sService
                                                        Shared.OpenApiServiceInformation = openApiService.OpenApiServiceInformation })
      |> List.ofSeq

    return
      { Shared.PlainK8sServices = clientFormatK8sOnlyServices
        Shared.OrnServices = clientFormatOrnServices
        Shared.Messages = feedbackAgent.Log |> Seq.toList }

  }
