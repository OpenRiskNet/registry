module OrnQueryTester.DereferencingVisitor

open Microsoft.OpenApi.Models
open Microsoft.OpenApi.Services
open System.Collections.Generic
open Microsoft.OpenApi.Interfaces

type DereferencingVisitor (components : OpenApiComponents) =
  inherit OpenApiVisitorBase()
  let Components = components

  /// <summary>
  /// Visits <see cref="OpenApiDocument"/>
  /// </summary>
  override self.Visit(doc : OpenApiDocument ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiInfo"/>
  /// </summary>
  override self.Visit(info : OpenApiInfo ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiContact"/>
  /// </summary>
  override self.Visit(contact : OpenApiContact ) =
    ()


  /// <summary>
  /// Visits <see cref="OpenApiLicense"/>
  /// </summary>
  override self.Visit(license : OpenApiLicense ) =
    ()

  /// <summary>
  /// Visits list of <see cref="OpenApiServer"/>
  /// </summary>
  override self.Visit(servers : IList<OpenApiServer> ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiServer"/>
  /// </summary>
  override self.Visit(server : OpenApiServer ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiPaths"/>
  /// </summary>
  override self.Visit(paths : OpenApiPaths ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiPathItem"/>
  /// </summary>
  override self.Visit(pathItem : OpenApiPathItem ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiServerVariable"/>
  /// </summary>
  override self.Visit(serverVariable : OpenApiServerVariable ) =
    ()

  /// <summary>
  /// Visits the operations.
  /// </summary>
  override self.Visit(operations : IDictionary<OperationType, OpenApiOperation> ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiOperation"/>
  /// </summary>
  override self.Visit(operation : OpenApiOperation ) =
    ()

  /// <summary>
  /// Visits list of <see cref="OpenApiParameter"/>
  /// </summary>
  override self.Visit(parameters : IList<OpenApiParameter> ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiParameter"/>
  /// </summary>
  override self.Visit(parameter : OpenApiParameter ) =
    match parameter.Reference with
    | null -> ()
    | reference ->
        printfn "Dereferencing: %s" reference.ReferenceV3
        let copyFromReference (parameter : OpenApiParameter) (dereferencedParameter : OpenApiParameter) =
          parameter.AllowEmptyValue <- dereferencedParameter.AllowEmptyValue
          parameter.AllowReserved <- dereferencedParameter.AllowReserved
          parameter.Content <- dereferencedParameter.Content
          parameter.Deprecated <- dereferencedParameter.Deprecated
          parameter.Description <- dereferencedParameter.Description
          parameter.Example <- dereferencedParameter.Example
          parameter.Examples <- dereferencedParameter.Examples
          parameter.Explode <- dereferencedParameter.Explode
          parameter.Extensions <- dereferencedParameter.Extensions
          parameter.In <- dereferencedParameter.In
          parameter.Name <- dereferencedParameter.Name
          parameter.Required <- dereferencedParameter.Required
          parameter.Schema <- dereferencedParameter.Schema
          parameter.Style <- dereferencedParameter.Style

          parameter.Reference <- null
          ()

        let (success, dereferenced) = Components.Parameters.TryGetValue(reference.Id)
        if success then
          copyFromReference parameter dereferenced
        else
          printfn "Could not dereference %s" reference.Id
        ()

  /// <summary>
  /// Visits <see cref="OpenApiRequestBody"/>
  /// </summary>
  override self.Visit(requestBody : OpenApiRequestBody ) =
    match requestBody.Reference with
    | null -> ()
    | reference ->
        printfn "Dereferencing: %s" reference.ReferenceV3
        let copyFromReference (requestBody : OpenApiRequestBody) (dereferencedRequestBody : OpenApiRequestBody) =
          requestBody.Content <- dereferencedRequestBody.Content
          requestBody.Description <- dereferencedRequestBody.Description
          requestBody.Extensions <- dereferencedRequestBody.Extensions
          requestBody.Reference <- dereferencedRequestBody.Reference
          requestBody.Required <- dereferencedRequestBody.Required

          requestBody.Reference <- null
          ()

        let (success, dereferenced) = Components.RequestBodies.TryGetValue(reference.Id)
        if success then
          copyFromReference requestBody dereferenced
        else
          printfn "Could not dereference %s" reference.Id
        ()

  /// <summary>
  /// Visits responses.
  /// </summary>
  //override self.Visit(responses : IDictionary<string, OpenApiResponse> ) =
//    ()

  /// <summary>
  /// Visits <see cref="OpenApiResponse"/>
  /// </summary>
  override self.Visit(response : OpenApiResponse ) =
    match response.Reference with
    | null -> ()
    | reference ->
        printfn "Dereferencing: %s" reference.Id
        let copyFromReference (response : OpenApiResponse) (dereferencedResponse : OpenApiResponse) =
          response.Content <- dereferencedResponse.Content
          response.Description <- dereferencedResponse.Description
          response.Extensions <- dereferencedResponse.Extensions
          response.Headers <- dereferencedResponse.Headers
          response.Links <- dereferencedResponse.Links
          response.Reference <- dereferencedResponse.Reference

          response.Reference <- null
          ()

        let (success, dereferenced) = Components.Responses.TryGetValue(reference.Id)
        if success then
          copyFromReference response dereferenced
        else
          printfn "Could not dereference %s" reference.Id
        ()

  /// <summary>
  /// Visits <see cref="OpenApiResponses"/>
  /// </summary>
  override self.Visit(response : OpenApiResponses ) =
    ()

  /// <summary>
  /// Visits media type content.
  /// </summary>
  override self.Visit(content : IDictionary<string, OpenApiMediaType> ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiMediaType"/>
  /// </summary>
  override self.Visit(mediaType : OpenApiMediaType ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiEncoding"/>
  /// </summary>
  override self.Visit(encoding : OpenApiEncoding ) =
    ()

  /// <summary>
  /// Visits the examples.
  /// </summary>
  override self.Visit(examples : IDictionary<string, OpenApiExample> ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiComponents"/>
  /// </summary>
  override self.Visit(components : OpenApiComponents ) =
    ()


  /// <summary>
  /// Visits <see cref="OpenApiComponents"/>
  /// </summary>
  override self.Visit(externalDocs : OpenApiExternalDocs ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiSchema"/>
  /// </summary>
  override self.Visit(schema : OpenApiSchema ) =

    match schema.Reference with
    | null -> ()
    | reference ->
        printfn "Dereferencing: %s" reference.ReferenceV3
        let copyFromReference (schema : OpenApiSchema) (dereferencedSchema : OpenApiSchema) =
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

        let (success, dereferenced) = Components.Schemas.TryGetValue(reference.Id)
        if success then
          copyFromReference schema dereferenced
        else
          printfn "Could not dereference %s" reference.Id
        ()


  /// <summary>
  /// Visits the links.
  /// </summary>
  override self.Visit(links : IDictionary<string, OpenApiLink> ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiLink"/>
  /// </summary>
  override self.Visit(link : OpenApiLink ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiCallback"/>
  /// </summary>
  override self.Visit(callback : OpenApiCallback ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiTag"/>
  /// </summary>
  override self.Visit(tag : OpenApiTag ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiHeader"/>
  /// </summary>
  override self.Visit(tag : OpenApiHeader ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiOAuthFlow"/>
  /// </summary>
  override self.Visit(openApiOAuthFlow : OpenApiOAuthFlow ) =
    ()

  /// <summary>
  /// Visits <see cref="OpenApiSecurityRequirement"/>
  /// </summary>
  override self.Visit(securityRequirement : OpenApiSecurityRequirement ) =
    ()

  /// <summary>
  /// Visits list of <see cref="OpenApiTag"/>
  /// </summary>
  override self.Visit(openApiTags : IList<OpenApiTag> ) =
    ()

  /// <summary>
  /// Visits <see cref="IOpenApiExtensible"/>
  /// </summary>
  override self.Visit(openApiExtensible : IOpenApiExtensible ) =
    ()
