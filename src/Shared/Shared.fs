namespace Shared

open System

type ApplicationStatus =
  | Offline
  | Running

type OnlineApiInfo =
  { OnlineOpenApiDefinition : Uri
    ServiceUri : Uri
    ServicePort : int }

type ApiStatus =
  | Offline
  | Running of OnlineApiInfo

type Api =
  { Status : ApiStatus
    OfflineOpenApiDefinition : Uri
  }

type OrnApplication =
  { Name : string
    Logo : Uri
    Description : string
    Status : ApplicationStatus
    Iri : Uri
    HelmChartUri : Uri
    Apis : Api list
    }

type Application =
  { Name : string
    Logo : string
    Description : string
    Status : ApplicationStatus }

module Route =
  /// Defines how routes are generated on server and mapped from client
  let builder typeName methodName =
    sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol for client and server
/// Every record field must have the type : 'a -> Async<'b> where 'a can also be `unit`
/// Add more such fields, implement them on the server and they be directly available on client
type IRegistryProtocol =
  { getInitModel : unit -> Async<Application list> }