module Orn.Registry.Client.Types

open Orn.Registry.Shared
open Fable.PowerPack.Fetch
open Orn.Registry

type IKeycloakPromise<'returnType> =
    abstract success: ('returnType -> Unit) -> IKeycloakPromise<'returnType>
    abstract error: (obj -> Unit) -> IKeycloakPromise<'returnType>

type IKeycloakPromiseWrapper<'returnType> =
    abstract promise: IKeycloakPromise<'returnType>

type IKeycloak =
    abstract init: Unit -> IKeycloakPromise<bool>
    abstract login: obj -> Unit
    abstract token: string
    abstract subject: string
    abstract updateToken: float -> IKeycloakPromise<bool>

type OntologySearchTerm =
    { Text: string
      OntologyTerm: string option
      TermSuggestions: string list }

type LoadingStatus<'success, 'error> =
    | NotRequested
    | InFlight
    | Success of 'success
    | Failed of 'error

type ActiveTab =
    | ServicesTab
    | SparqlQueryTab
    | ExternalServices

let AllTabs = [ ServicesTab; SparqlQueryTab; ExternalServices ]

let tabToLabel =
    function
    | ServicesTab -> "Services"
    | SparqlQueryTab -> "SparQL query"
    | ExternalServices -> "External services"

type AuthToken = AuthToken of string

type LoginInfo =
    { Token: AuthToken
      UserId: string }

type AppModel =
    { Services: LoadingStatus<Shared.ActiveServices, string>
      InputSearchTerm: OntologySearchTerm
      OutputSearchTerm: OntologySearchTerm
      SparqlQuery: string
      SparqlResults: LoadingStatus<SparqlResultsForServices, string>
      ActiveTab: ActiveTab
      ExternalServiceTextFieldContent: string
      ExternalServiceListTextFieldContent: string
      SelectedSparqlService: string
      LoginInfo: LoginInfo
      SelectedExampleSparqlQuery : string }

type Model =
    | Authenticating
    | LoggedIn of AppModel
    | LoginError of string

type AppMsg =
    | Refresh of Result<Shared.ActiveServices, exn>
    | RunSparqlQuery
    | SparqlQueryFinished of Result<SparqlResultsForServices, exn>
    | Awake
    | QueryChanged of string
    | TabChanged of ActiveTab
    | AddExternalService
    | RemoveExternalService of string
    | ExternalServiceTextFieldChanged of string
    | AddExternalServiceRequestCompleted of Result<Response, exn>
    | RemoveExternalServiceRequestCompleted of Result<Response, exn>
    | AddExternalServiceList
    | RemoveExternalServiceList of string
    | ExternalServiceListTextFieldChanged of string
    | AddExternalServiceListRequestCompleted of Result<Response, exn>
    | RemoveExternalServiceListRequestCompleted of Result<Response, exn>
    | SparqlSerivceSelected of string
    | SparqlExampleQuerySelected of string

type Msg =
    | AppMessage of AppMsg
    | KeycloakInit of Result<LoginInfo, obj>

let exampleQueries =
    [ "Select title inside top level info object"
      , """PREFIX orn: <http://openrisknet.org/schema#>
SELECT ?title
WHERE {
?tool orn:info ?info.
?info orn:title ?title
}
"""

      "Select all triples"
      , """PREFIX orn: <http://openrisknet.org/schema#>
SELECT * {?s ?p ?o}"""

      "Select the first 100 triples that use a predicate not in the default openrisknet.org namespace"
      , """PREFIX orn: <http://openrisknet.org/schema#>
SELECT ?s ?p ?o
WHERE {
    ?s ?p ?o .
  FILTER (!(strstarts(str(?p), 'http://openrisknet.org')))
}
LIMIT 100"""

      "Select all endpoints that at some point below in their structure use the CHEMINF_000018 term (smiles)"
      , """PREFIX orn: <http://openrisknet.org/schema#>
SELECT * { ?s1 orn:paths ?o1 .
?o1 (orn:blank|!orn:blank)* ?o2 .
?o2 <http://semanticscience.org/resource/CHEMINF_000018> ?o}"""

      "Select all endpoints that use OpenRiskNet highlevel expects and returns annotations",
      """# Get all the endpoints that have an openrisknet highlevel description
# using "expects" or "returns" to describe their input and output

PREFIX orn: <http://openrisknet.org/schema#>
SELECT ?endpointnode ?relativeendpointpath ?endpointverbnode ?expects ?returns
{
# start query at the "paths" predicate
?n1 orn:paths ?n2 .

# one level down are the endpoints - we select the relative endpoint path (e.g. "/predict")
# here but this looks a bit weird as it is an IRI with the usual http://openrisknet.org/schema# prefix
# (e.g. "http://openrisknet.org/schema#/predict")
# If service authors opt to use x-orn-@id on their endpoints (or endpoint verbs), they can map the IRI
# of the node to the correct URL of the service endpoint so it can be queried there directly - but this
# only works if the id has been assigned correctly by the service author, otherwise you get an anonymous node id
?n2 ?relativeendpointpath ?endpointnode.
?endpointnode ?verb ?endpointverbnode.

# Now select the "highlevel" annotations for openrisknet services, the orn:expects and orn:returns
# predicates that describe the inputs and outputs of an endpoint on a high level. Note that because
# of how UNION works in Sparql a single endpoint that has both will be returned twice, once with the
# expects result variable set and once with the returns variable set
 { ?endpointverbnode orn:expects ?expects }
    UNION
 { ?endpointverbnode orn:returns ?returns}

}"""

      "Get all endpoints that use one of two populare ontology terms for SMILES in their json response body",
      """# Select endpoints that somewhere in their response have a json key that is mapped
# to one of two common ontology terms for SMILES (EDAM ontology term format_1196 or CHEMINF ontology term 000018)

PREFIX orn: <http://openrisknet.org/schema#>
SELECT ?endpointnode ?relativeendpointpath ?endpointverbnode
{ # start query at the "paths" predicate
  ?n1 orn:paths ?n2 .

# one level down are the endpoints - we select the relative endpoint path
# here but this looks a bit weird as it is an IRI with the usual openrisknet.org/schema# prefix.
# Alternatively we also select the endpoint node so the service annotators can use x-orn-@id
# to tag the endpoint (or the verb below) to set the IRI to the correct URL where the endpoint can be queried
?n2 ?relativeendpointpath ?endpointnode.
?endpointnode ?verb ?endpointverbnode.

# select the responses, only the status code 200 response
?endpointverbnode orn:responses ?responses.
?responses orn:200 ?response200.

# Now traverse from here an arbitrary depth into the triple graph. This is computationally
# expensive so the further "down" in the graph you can start, the better
?response200 (orn:blank|!orn:blank)* ?n3.

# finally require that somewhere down in the graph at a node ?n3 we have a connection with
# either the CHEMINF_000018 ontology term as a predicate or the format_1196 ontology term
# (both common ontololgy terms for SMILES
 { ?n3 <http://semanticscience.org/resource/CHEMINF_000018> ?n4 }
    UNION
 { ?n3 <http://edamontology.org/format_1196> ?n4 }
}"""

      "Select all endponts that use a query parameter with IRI CHEMINF_000018 (smiles)",
      """# Select all endoints that use a query parameter that was assigned the CHEMINF_000018 id
# (smiles). Not that for parameters the mapping of json keys to ontology terms in
# the x-onr-@context section does not do anything because the name of the parameter is
# not specified as a json key but is just a string value. The author of a service has to
# explicitly annotate a parameter with x-orn-@id tag of the ontology term in question for
# this to work.

PREFIX orn: <http://openrisknet.org/schema#>
SELECT ?endpointnode ?relativeendpointpath ?endpointverbnode
{
# start query at the "paths" predicate
?n1 orn:paths ?n2 .

# one level down are the endpoints - we select the predicate which is the relative endpoint path (e.g. "/predict")
# here but this looks a bit weird as it is an IRI with the usual http://openrisknet.org/schema# prefix
# (e.g. "http://openrisknet.org/schema#/predict")
# If service authors opt to use x-orn-@id on their endpoints (or endpoint verbs), they can map the IRI
# of the node to the correct URL of the service endpoint so it can be queried there directly - but this
# only works if the id has been assigned correctly by the service author, otherwise you get an anonymous node id
?n2 ?relativeendpointpath ?endpointnode.
?endpointnode ?verb ?endpointverbnode.

# Go one level down into the parameters section and match if the id of the parameter
# is the ontology term that we are looking for. Alternatively you could match based on
# text or a regex here so you can find services where the parameter has not been tagged
# with this ontology term but then you rely on text based matching.
?endpointverbnode orn:parameters <http://semanticscience.org/resource/CHEMINF_000018>
}"""
    ]
