module Orn.Registry.JsonLdParsing

open System.IO
open VDS.RDF.Parsing
open VDS.RDF.Query
open VDS.RDF.Query.Datasets
open VDS.RDF
open VDS.RDF.Writing.Formatting
open System
open Newtonsoft.Json
open Newtonsoft.Json.Linq
open Cvdm.ErrorHandling
open Orn.Registry.BasicTypes

// OpenAPI cannot contain arbitrary keys - instead custom properties need
// to be prefixed with "x-". At the same time, Json-LD needs a json-ld context
// that is given as a json key of "@context". We want the document to be valid
// OpenAPI but also want to use it as Json-LD - so here we rename every "x-orn-@context"
// to "@context"
let fixOrnJsonLdContext (OpenApiDereferenced openapiString) =
    let rec recursiveRenameContext (token : Chiron.Json) =
        match token with
        | Chiron.Array items -> items |> List.map recursiveRenameContext |> Chiron.Array
        | Chiron.Object keyValPairs ->
            keyValPairs
            |> Map.toSeq
            |> Seq.map (fun (key, value) ->
                           match key with
                           | "x-orn-@context" -> ("@context", recursiveRenameContext value)
                           | _ -> (key, recursiveRenameContext value)
                           )
            |> Map.ofSeq
            |> Chiron.Object
        | simpleValue -> simpleValue

    let resultFromChoice choice =
            match choice with
            | Choice1Of2 a -> Ok a
            | Choice2Of2 b -> Error b

    openapiString
    |> Chiron.Parsing.Json.tryParse
    |> resultFromChoice
    |?> recursiveRenameContext
    |?> Chiron.Formatting.Json.format
    |?> OpenApiFixedContextEntry

let loadJsonLdIntoTripleStore
    (OpenApiFixedContextEntry openapiString)
    : (Result<TripleStore, string>) =
    result {
        let store = new TripleStore()

        use reader = new StringReader(openapiString)
        let parser = JsonLdParser()

        parser.Load(store, reader)
        return store
    }


let createSparqlQuery (queryString : string) : Result<SparqlQuery, string> =
    let sparqlParser = SparqlQueryParser()
    try
        let query = sparqlParser.ParseFromString(queryString)
        Ok query
    with
    | :? VDS.RDF.Parsing.RdfParseException as ex ->
        Error (sprintf "Error parsing the Sparql query: %s" ex.Message)


let runQuery (store : TripleStore) (query : SparqlQuery) =
    let ds = InMemoryDataset(store, true)

    let processor = LeviathanQueryProcessor(ds)

    //Then we can parse a SPARQL string into a query
    try
        let resultSet = processor.ProcessQuery(query)

        match resultSet with
        | :? SparqlResultSet as r ->
            match r.ResultsType with
            | VDS.RDF.Query.SparqlResultsType.Boolean -> Ok (Orn.Registry.Shared.BooleanResult r.Result)
            | VDS.RDF.Query.SparqlResultsType.VariableBindings ->
                r.Results
                |> Seq.map
                    (fun result ->
                        query.Variables
                        |> Seq.map (fun var -> if result.HasValue(var.Name) then result.[var.Name].ToString() else "")
                        |> Seq.toList
                    )
                |> Seq.toList
                |> fun results -> Orn.Registry.Shared.BindingResult { Variables = query.Variables |> Seq.map (fun var -> var.Name) |> List.ofSeq; ResultValues = results}
                |> Ok
            | _ -> Error "Unknown SPARQL result type"
        | _ ->
            Error "Item was not of expected type"
    with
    | :? VDS.RDF.Parsing.RdfParseException as ex ->
        Error (sprintf "Error parsing the Sparql query: %s" ex.Message)
