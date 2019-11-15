module Orn.Registry.OpenApiTransformer
// #r @"../packages/Microsoft.OpenApi/lib/netstandard2.0/Microsoft.OpenApi.dll";;
// #r @"../packages/Microsoft.OpenApi.Readers/lib/netstandard2.0/Microsoft.OpenApi.Readers.dll";;
// #r @"../packages/SharpYaml/lib/netstandard1.6/SharpYaml.dll";;
// #load "OpenApiTransformer.fs";;
open System.IO
open Microsoft.OpenApi.Models
open Microsoft.OpenApi.Readers
open Microsoft.OpenApi.Writers
open Microsoft.OpenApi.Services
open Microsoft.OpenApi.Validations
open System.Text
open OrnQueryTester.DereferencingVisitor
open Orn.Registry.BasicTypes
open Orn.Registry
open Orn.Registry.Shared

let DereferenceOpenApi(openapiDocument: Microsoft.OpenApi.Models.OpenApiDocument): Microsoft.OpenApi.Models.OpenApiDocument =
    let visitor = DereferencingVisitor(openapiDocument.Components)
    let walker = OpenApiWalker(visitor)
    walker.Walk(openapiDocument)
    openapiDocument

let ParseAndDereferenceOpenApi(OpenApiRaw openApiYaml) =
    let openApiAsBytes = System.Text.Encoding.UTF8.GetBytes(openApiYaml)
    use stream = new MemoryStream(openApiAsBytes)
    let reader = OpenApiStreamReader()
    let openapi, diagnostics = reader.Read(stream)
    if Seq.isEmpty diagnostics.Errors then
        let validator = OpenApiValidator(Microsoft.OpenApi.Validations.ValidationRuleSet.GetDefaultRuleSet())
        validator.Visit(openapi)
        if (Seq.isEmpty validator.Errors) then
            let dereferenced = DereferenceOpenApi openapi
            Ok dereferenced
        else Error(sprintf "%A" validator.Errors)
    else Error(sprintf "%A" diagnostics.Errors)

let TransformOpenApiToV3Dereferenced retrievedAt openApiUrl openApiString =
    result {
        let! openapi = ParseAndDereferenceOpenApi openApiString
        use writer = new StringWriter()
        let openapiWriter = OpenApiJsonWriter(writer)
        // TODO: extract more useful information (endpoints? tags?)
        let endpoints = openapi.Paths.Keys

        let description =
            { Description = if isNull openapi.Info.Description then "" else openapi.Info.Description
              Endpoints = endpoints |> List.ofSeq
              OpenApiUrl = openApiUrl
              Name = openapi.Info.Title
              RetrievedAt = retrievedAt }
        try
            openapi.SerializeAsV3 openapiWriter
            return description, OpenApiDereferenced(writer.ToString())
        with ex -> return! Error(ex.ToString())
    }

let testOpenAPI = """
# ChemIdConvert - Chemical identifier conversion service
swagger: '2.0'
info:
  title: ChemIdConvert - Chemical identifier conversion service
  description: This REST Api allows you to submit chemical identifiers in one format and translate it into another format (e.g. SMILES -> InChi)
  version: "1.0.0"
# the domain of the service - in our use-case somewhat nonsensical but mandatory
host: chemidconvert
# array of all schemes that your API supports
schemes:
  - https
# will be prefixed to all paths
basePath: /v1
produces:
  - application/json
paths:
  /asSvg:
    get:
      summary: Returns an svg text of the given molecule
      produces:
        - image/svg+xml
      parameters:
        - $ref: "#/parameters/smiles"
        - name: width
          in: query
          type: integer
          description: SVG image width
          required: false
          default: 450
        - name: height
          in: query
          type: integer
          description: SVG image height
          required: false
          default: 450
      responses:
        200:
          description: Successfully created an svg represenatation
          schema:
            type: string
            description: The text representation of the svg of the molecule
        default:
          description: Unexpected error
          schema:
            $ref: '#/definitions/simpleError'
  /molWeight:
    get:
      summary: Returns the molecular weight (calculated with rdkit)
      parameters:
        - $ref: "#/parameters/smiles"
      responses:
        200:
          $ref: "#/responses/molWeight"
        default:
          description: Unexpected error
          schema:
            $ref: '#/definitions/simpleError'
  /smiles/to/inchi:
    get:
      summary: Converts from a SMILES format into InChI format
      description: >
        Converts from a SMILES format into InChI format. Only a single match is
        returned if there is an ambiguity in the conversion. The conversion is
        performed in process via rdkit.
      tags:
        - SMILES
        - InChI
      parameters:
        - $ref: "#/parameters/smiles"
      responses:
        200:
          $ref: "#/responses/inchi"
        400:
          description: Invalid request (typically a mandatory parameter was missing or malformed)
          schema:
            $ref: '#/definitions/simpleError'
        default:
          description: Unexpected error
          schema:
            $ref: '#/definitions/simpleError'
parameters:
  smiles:
    name: smiles
    in: query
    type: string
    format: smiles
    description: Compound structure notated using SMILES notation
    required: true
    x-ontology: http://edamontology.org/format_1196
  inchi:
    name: inchi
    in: query
    type: string
    description: Compound structure notated using InChI notation
    required: true
    x-ontology: http://edamontology.org/format_1197
  inchikey:
    name: inchikey
    in: query
    type: string
    description: Compound structure notated using InChIKey notation
    required: true
    x-ontology: http://edamontology.org/format_1199
  cas:
    name: cas
    in: query
    type: string
    description: Compound structure notated as a CAS number (3 groups of numbers separated by dashes)
    required: true
    x-ontology: http://edamontology.org/data_3102
responses:
  inchi:
    description: The converted InChI string
    schema:
      type: object
      properties:
        inchi:
          type: string
          description: Compound structure notated using InChI notation
          x-ontology: http://edamontology.org/format_1197
  smiles:
    description: The converted SMILES string
    schema:
      type: object
      properties:
        smiles:
          type: string
          description: Compound structure notated using (canonical) SMILES notation
          x-ontology: http://edamontology.org/format_1196
  inchikey:
    description: The converted InChIKey string
    schema:
      type: object
      properties:
        inchikey:
          type: string
          description: Compound structure notated using InChIKey
          x-ontology: http://edamontology.org/format_1199
  cas:
    description: The converted CAS number (3 groups of numbers separated by dashes)
    schema:
      type: object
      properties:
        cas:
          type: string
          description: Compound structure notated using a CAS number
          x-ontology: http://edamontology.org/data_3102
  molWeight:
    description: The molecular weight (caluclated with rdkit)
    schema:
      type: object
      properties:
        smiles:
          type: string
          description: Compound structure notated using (canonical) SMILES notation
          x-ontology: http://edamontology.org/format_1196
        molWeight:
          type: number
          description: The molecular weight (caluclated with rdkit)
definitions:
  simpleError:
    type: string
"""
