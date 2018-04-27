module Orn.Registry.BasicTypes

type OpenApiRaw = OpenApiRaw of string
type OpenApiDereferenced = OpenApiDereferenced of string
type OpenApiFixedContextEntry = OpenApiFixedContextEntry of string
type SparqlQuery = SparqlQuery of string
type SwaggerUrl = SwaggerUrl of string

type Agent<'t> = MailboxProcessor<'t>
type ServiceIdentifier = ServiceIdentifier of string
type LabelKey = string

let (|?>) a b = a |> (Result.map b)
