namespace Orn.Registry.Shared

open System

type Url = string
type SparqlQuery = SparqlQuery of string
type SwaggerUrl = SwaggerUrl of string

module Constants =
  [<Literal>]
  let OpenApiLabelStaticServices = "openrisknet-static-services"
  let OpenApiLabelDynamicServices = "openrisknet-dynamic-services"
  let KubernetesNamespace = "openrisknet"
// Application
// type ApplicationStatus =
//   | Offline
//   | Running

// type ApplicationType =
//   | Deployable of status:  ApplicationStatus * helmChartUri : Url
//   | NonDeyloyable of publicServiceUri : Url

// type OrnApplication =
//   { Name : string
//     Logo : Url
//     Description : string
//     ApplicationType : ApplicationType
//     Id : Url
//     }

// type DynamicServiceInformationSource =
//   { DynamicServiceSource : string
//     LastPoll : DateTime
//   }

type Feedback =
  | OpenApiDownloadFailed of SwaggerUrl
  | OpenApiParsingFailed of SwaggerUrl * string
  | JsonLdContextMissing of SwaggerUrl
  | JsonLdParsingError of SwaggerUrl * string

type TimestampedFeedback =
  { Feedback : Feedback
    Timestamp : System.DateTime }

// Service
type K8sService =
  { //OnlineOpenApiDefinition : Url
    Name : string // extracted from the openapi definition
    ServicePorts : int array }

type OpenApiServiceInformation =
  { Description : string
    Endpoints : string list
    SwaggerUrl : SwaggerUrl
  }

type OrnService =
  { K8sService : K8sService
    OpenApiServiceInformation : OpenApiServiceInformation
  }

type ActiveServices =
  { PlainK8sServices : K8sService list
    OrnServices : OrnService list
    Messages : TimestampedFeedback list }

// Search
type ServiceSqarqlQueryResult =
  { Name : String
    Endpoint : Url }


module Route =
  /// Defines how routes are generated on server and mapped from client
  let builder typeName methodName =
    sprintf "/api/%s/%s" typeName methodName

/// A type that specifies the communication protocol for client and server
/// Every record field must have the type : 'a -> Async<'b> where 'a can also be `unit`
/// Add more such fields, implement them on the server and they be directly available on client
type IRegistryProtocol =
  { getCurrentServices : unit -> Async<ActiveServices> }
