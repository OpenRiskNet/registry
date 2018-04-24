open System.IO
open System.Net

open Suave
open Suave.Operators

open Fable.Remoting.Server
open Fable.Remoting.Suave

open Shared
let clientPath = Path.Combine("..","Client") |> Path.GetFullPath
let port = 8085us

let config =
  { defaultConfig with
      homeFolder = Some clientPath
      bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port ] }

let getCurrentServices () : Async<Service list> =
  async {
    let dummyServices =
        [{ OnlineOpenApiDefinition = "http://chemidconvert:8080/swagger.json"
           Name = "ChemiDConvert"
           ServiceUri = "http://chemidconvert/"
           ServicePort = 8080 } ]
    return dummyServices
  }

let init : WebPart =
  let registryProcotol =
    { getCurrentServices = getCurrentServices }
  // Create a WebPart for the given implementation of the protocol
  remoting registryProcotol {
    // define how routes are mapped
    use_route_builder Route.builder
  }

let webPart =
  choose [
    init
    Filters.path "/" >=> Files.browseFileHome "index.html"
    Files.browseHome
    RequestErrors.NOT_FOUND "Not found!"
  ]

startWebServer config webPart
