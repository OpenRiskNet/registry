module Orn.Registry.OpenApiDereferenceLogic

open Microsoft.OpenApi.Models
open Microsoft.OpenApi.Services
open System.Collections.Generic
open Microsoft.OpenApi.Interfaces

let private copyFromReference (schema : OpenApiSchema) (dereferencedSchema : OpenApiSchema) =
  schema.AdditionalProperties <- dereferencedSchema.AdditionalProperties
  schema.AllOf <- dereferencedSchema.AllOf
  schema.AnyOf <- dereferencedSchema.AnyOf
  schema.Default <- dereferencedSchema.Default
  schema.Deprecated <- dereferencedSchema.Deprecated
  schema.Description <- dereferencedSchema.Description
  schema.Discriminator <- dereferencedSchema.Discriminator
  schema.Enum <- dereferencedSchema.Enum
  schema.Example <- dereferencedSchema.Example
  schema.ExclusiveMaximum <- dereferencedSchema.ExclusiveMaximum
  schema.ExclusiveMinimum <- dereferencedSchema.ExclusiveMinimum
  schema.Extensions <- dereferencedSchema.Extensions
  schema.ExternalDocs <- dereferencedSchema.ExternalDocs
  schema.Format <- dereferencedSchema.Format
  schema.Items <- dereferencedSchema.Items
  schema.Maximum <- dereferencedSchema.Maximum
  schema.MaxItems <- dereferencedSchema.MaxItems
  schema.MaxLength <- dereferencedSchema.MaxLength
  schema.MinProperties <- dereferencedSchema.MinProperties
  schema.MultipleOf <- dereferencedSchema.MultipleOf
  schema.Not <- dereferencedSchema.Not
  schema.Nullable <- dereferencedSchema.Nullable
  schema.OneOf <- dereferencedSchema.OneOf
  schema.Pattern <- dereferencedSchema.Pattern
  schema.Properties <- dereferencedSchema.Properties
  schema.ReadOnly <- dereferencedSchema.ReadOnly
  schema.Required <- dereferencedSchema.Required
  schema.Title <- dereferencedSchema.Title
  schema.Type <- dereferencedSchema.Type
  schema.UniqueItems <- dereferencedSchema.UniqueItems
  schema.WriteOnly <- dereferencedSchema.WriteOnly
  schema.Xml <- dereferencedSchema.Xml

  schema.Reference <- null
  ()

let private copyFromParameterReference (target : OpenApiParameter) (source : OpenApiParameter) =
  target.AllowEmptyValue <- source.AllowEmptyValue
  target.AllowReserved <- source.AllowReserved
  target.Content <- source.Content
  target.Deprecated <- source.Deprecated
  target.Description <- source.Description
  target.Example <- source.Example
  target.Examples <- source.Examples
  target.Explode <- source.Explode
  target.Extensions <- source.Extensions
  target.In <- source.In
  target.Name <- source.Name
  target.Required <- source.Required
  target.Schema <- source.Schema
  target.Style <- source.Style
  target.UnresolvedReference <- source.UnresolvedReference

  target.Reference <- null
  ()

let private copyFromResponseReference (target : OpenApiResponse) (source : OpenApiResponse) =
    target.Content <- source.Content
    target.Description <- source.Description
    target.Extensions <- source.Extensions
    target.Headers <- source.Headers
    target.Links <- source.Links
    target.UnresolvedReference <- source.UnresolvedReference

    target.Reference <- null
    ()

let private copyFromRequestBodyReference (target : OpenApiRequestBody) (source : OpenApiRequestBody) =
    target.Content <- source.Content
    target.Description <- source.Description
    target.Extensions <- source.Extensions
    target.Required <- source.Required
    target.UnresolvedReference <- source.UnresolvedReference

    target.Reference <- null
    ()


type RecursionDepth =
    | Root
    | Deeper

let rec private collectDependencies (recursionDepth : RecursionDepth) (schema : OpenApiSchema) =
    // For some bizarre reason the Reference field is set for the root items in Compounds.Schemas.*
    // sort of pointing at itself even tough there are values present. In the spec it is clearly a sum
    // type of EITHER something is a reference or a proper value but whatever. The code below
    // ignores the reference on the root level and just recurses deeper, in other cases when we
    // get a reference we just return the id and stop recursing
    match schema.Reference, recursionDepth with
    | null, _ ->
        seq {
            yield! (schema.OneOf |> Seq.collect (collectDependencies Deeper))
            yield! (schema.AllOf |> Seq.collect (collectDependencies Deeper))
            yield! (schema.AnyOf |> Seq.collect (collectDependencies Deeper))
            yield! (schema.Properties.Values |> Seq.collect (collectDependencies Deeper))
            if not (isNull schema.Not) then yield! (schema.Not |> (collectDependencies Deeper))
            if not (isNull schema.Items) then yield! (schema.Items |> (collectDependencies Deeper))
            if not (isNull schema.AdditionalProperties) then yield! (schema.AdditionalProperties |> (collectDependencies Deeper))
        }
    | reference, Root ->
         seq {
            yield! (schema.OneOf |> Seq.collect (collectDependencies Deeper))
            yield! (schema.AllOf |> Seq.collect (collectDependencies Deeper))
            yield! (schema.AnyOf |> Seq.collect (collectDependencies Deeper))
            yield! (schema.Properties.Values |> Seq.collect (collectDependencies Deeper))
            if not (isNull schema.Not) then yield! (schema.Not |> (collectDependencies Deeper))
            if not (isNull schema.Items) then yield! (schema.Items |> (collectDependencies Deeper))
            if not (isNull schema.AdditionalProperties) then yield! (schema.AdditionalProperties |> (collectDependencies Deeper))
        }
    | reference, Deeper ->
        Seq.singleton (reference.Id)

let rec private dereferenceRecusively (dereferencedSoFar : #IDictionary<string, OpenApiSchema>) (schema : OpenApiSchema) =
    match schema.Reference with
    | null ->
        schema.OneOf |> Seq.iter (dereferenceRecusively dereferencedSoFar )
        schema.AllOf |> Seq.iter (dereferenceRecusively dereferencedSoFar )
        schema.AnyOf |> Seq.iter (dereferenceRecusively dereferencedSoFar )
        schema.Properties.Values |> Seq.iter (dereferenceRecusively dereferencedSoFar)
        if not (isNull schema.Not) then dereferenceRecusively dereferencedSoFar schema.Not
        if not (isNull schema.Items) then dereferenceRecusively dereferencedSoFar schema.Items
        if not (isNull schema.AdditionalProperties) then dereferenceRecusively dereferencedSoFar schema.AdditionalProperties
        ()
    | reference ->
        if dereferencedSoFar.ContainsKey(reference.Id) then
            copyFromReference schema dereferencedSoFar.[reference.Id]
        ()


let private dereferenceComponentSchemas (components : OpenApiComponents) =
    printfn "Collecting dependencies"
    let mutable componentKeysAndDependencies =
        components.Schemas.Keys
        |> Seq.map (fun key -> key, Set.ofSeq <| collectDependencies Root components.Schemas.[key])
        |> Seq.toArray

    printfn "Dependencies collected"
    printfn "%A" componentKeysAndDependencies
    let fullyDereferenced = new Dictionary<string, OpenApiSchema>()
    let mutable didWork = true

    while didWork do
        let noFurtherDeps, remainingDeps = componentKeysAndDependencies |> Array.partition (fun (key, deps) -> deps.Count = 0)

        let noFurtherDeps =
            noFurtherDeps
            |> Array.map fst
            |> Set.ofArray

        for key in noFurtherDeps do
            let schemaExists, schema = components.Schemas.TryGetValue key
            if schemaExists then
               fullyDereferenced.Add(key, schema)

        for keyValuePair in components.Schemas do
            dereferenceRecusively fullyDereferenced keyValuePair.Value

        let remainingDeps =
            remainingDeps
            |> Array.map (fun (key, deps) -> key, Set.difference deps noFurtherDeps)

        componentKeysAndDependencies <- remainingDeps

        didWork <-  noFurtherDeps |> Set.isEmpty |> not
    fullyDereferenced

let private dereferenceTopLevelSchema (fullyDereferencedSchemas : #IDictionary<string, OpenApiSchema>) (allSchemas : #IDictionary<string, OpenApiSchema>) (target : OpenApiSchema) =
  if isNull target.Reference then
    dereferenceRecusively fullyDereferencedSchemas target
  else
    let schemaFromComponents = allSchemas.[target.Reference.Id]
    copyFromReference target schemaFromComponents

let private dereferenceParameters (fullyDereferenced : #IDictionary<string, OpenApiSchema>) (components : OpenApiComponents) (parameters : IList<OpenApiParameter>) =
    for parameter in parameters do
        if not (isNull parameter.Reference) then
            copyFromParameterReference parameter (components.Parameters.[parameter.Reference.Id])
        dereferenceTopLevelSchema fullyDereferenced (components.Schemas) parameter.Schema

let private dereferenceRequestBody (fullyDereferenced : #IDictionary<string, OpenApiSchema>) (components : OpenApiComponents) (requestBody : OpenApiRequestBody) =
    if not (isNull requestBody.Reference) then
        copyFromRequestBodyReference requestBody (components.RequestBodies.[requestBody.Reference.Id])
    for mediaType in requestBody.Content.Values do
        dereferenceTopLevelSchema fullyDereferenced (components.Schemas) mediaType.Schema

let private dereferenceResponses (fullyDereferenced : #IDictionary<string, OpenApiSchema>) (components : OpenApiComponents) (responses : OpenApiResponses) =
    for response in responses.Values do
        if not (isNull response.Reference) then
            copyFromResponseReference response (components.Responses.[response.Reference.Id])
        for mediaType in response.Content.Values do
            dereferenceTopLevelSchema fullyDereferenced components.Schemas mediaType.Schema

let dereferenceOpenApi (openapi : OpenApiDocument) =
    let fullyDereferenced = dereferenceComponentSchemas openapi.Components

    for keyValuePair in openapi.Components.Parameters do
        dereferenceTopLevelSchema fullyDereferenced (openapi.Components.Schemas) keyValuePair.Value.Schema
    for keyValuePair in openapi.Components.Responses do
        for mediaType in keyValuePair.Value.Content.Values do
            dereferenceTopLevelSchema fullyDereferenced (openapi.Components.Schemas) mediaType.Schema
    for keyValuePair in openapi.Components.RequestBodies do
        for mediaType in keyValuePair.Value.Content.Values do
            dereferenceTopLevelSchema fullyDereferenced (openapi.Components.Schemas) mediaType.Schema
    for path in openapi.Paths.Values do
        dereferenceParameters fullyDereferenced openapi.Components path.Parameters
        for operation in path.Operations.Values do
            dereferenceParameters fullyDereferenced openapi.Components operation.Parameters
            dereferenceRequestBody fullyDereferenced openapi.Components operation.RequestBody
            dereferenceResponses fullyDereferenced openapi.Components operation.Responses
