#load @"../../.paket/load/netcoreapp3.0/Server/Microsoft.OpenApi.fsx"
#load @"../../.paket/load/netcoreapp3.0/Server/SharpYaml.fsx"
#load @"../../.paket/load/netcoreapp3.0/Server/Microsoft.OpenApi.Readers.fsx"
#load @"DereferencingVisitor.fs"
let filepath = @"C:\Users\danyx\Documents\jaqpot.json"
let reader = Microsoft.OpenApi.Readers.OpenApiStreamReader()
let stream = System.IO.File.OpenRead(filepath)
let openapi, diagnostics = reader.Read(stream)
let dereferenced = OrnQueryTester.DereferencingVisitor.dereferenceComponentSchemas openapi.Components
printfn "Dereferenced all"
let outputpath = @"C:\Users\danyx\Documents\jaqpot-dereferenced.json"
let writer = new System.IO.StreamWriter(outputpath)
let openapiWriter = Microsoft.OpenApi.Writers.OpenApiJsonWriter(writer)
openapi.SerializeAsV3(openapiWriter)
