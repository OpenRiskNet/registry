module Orn.Registry.Server

open Suave
open Suave.Operators

open Fable.Remoting.Server
open Fable.Remoting.Suave

open Orn.Registry.Domain

let config =
  { defaultConfig with
      homeFolder = Some clientPath
      bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port ] }


let init : WebPart =
  let registryProcotol =
    { Shared.getCurrentServices = getCurrentServices }

  Remoting.createApi()
    |> Remoting.fromValue registryProcotol
    |> Remoting.buildWebPart


let webPart =
  choose [
    init
    Filters.path "/" >=> Files.browseFileHome "index.html"
    Files.browseHome
    RequestErrors.NOT_FOUND "Not found!"
  ]

startWebServer config webPart
