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

type SearchStatus =
    | NotStarted
    | Loading
    | Results of Shared.ServiceSqarqlQueryResult list

type ServiceList =
    | ServicesLoading
    | ServicesError of string
    | Services of Shared.ActiveServices

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
    { Services: ServiceList
      InputSearchTerm: OntologySearchTerm
      OutputSearchTerm: OntologySearchTerm
      SparqlQuery: string
      SparqlResults: SparqlResultsForServices option
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
      "Select all endpoints that at some point below then use the CHEMINF_000018 term (smiles)"
      , """PREFIX orn: <http://openrisknet.org/schema#>
SELECT * { ?s1 orn:paths ?o1 .
?o1 (orn:blank|!orn:blank)* ?o2 .
?o2 <http://semanticscience.org/resource/CHEMINF_000018> ?o}"""

    ]
