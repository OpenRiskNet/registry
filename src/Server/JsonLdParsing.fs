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


let runQuery (store : TripleStore) (Orn.Registry.Shared.SparqlQuery sparqlQuery) =
    let ds = InMemoryDataset(store, true);

    let processor = LeviathanQueryProcessor(ds);

    let sparqlParser = SparqlQueryParser()

    //Then we can parse a SPARQL string into a query
    try
        let query = sparqlParser.ParseFromString(sparqlQuery)
        let resultSet = processor.ProcessQuery(query)

        match resultSet with
        | :? SparqlResultSet as r ->
            r.Results
            |> Seq.map
                (fun result ->
                    query.Variables
                    |> Seq.map (fun var -> (var.Name, if result.HasValue(var.Name) then result.[var.Name].ToString() else ""))
                    |> Seq.toList
                )
            |> Seq.toList
            |> Ok
        | _ ->
            Error "Item was not of expected type"
    with
    | :? VDS.RDF.Parsing.RdfParseException as ex ->
        Error (sprintf "Error parsing the Sparql query: %s" ex.Message)
