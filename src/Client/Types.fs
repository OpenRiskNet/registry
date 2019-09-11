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
      SelectedSparqlService: string
      LoginInfo: LoginInfo }

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
    | SparqlSerivceSelected of string

type Msg =
    | AppMessage of AppMsg
    | KeycloakInit of Result<LoginInfo, obj>
