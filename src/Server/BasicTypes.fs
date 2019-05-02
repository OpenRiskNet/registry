module Orn.Registry.BasicTypes

type OpenApiRaw = OpenApiRaw of string
type OpenApiDereferenced = OpenApiDereferenced of string

type OpenApiFixedContextEntry =
    | OpenApiFixedContextEntry of string
    member this.Unwrap() =
        match this with
        | OpenApiFixedContextEntry content -> content

type Agent<'t> = MailboxProcessor<'t>
type ServiceIdentifier = ServiceIdentifier of string
type LabelKey = string

let (|?>) a b = a |> (Result.map b)
