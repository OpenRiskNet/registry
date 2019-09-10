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


[<Measure>]
type millisecond

let millisecondsPerSecond = 1000.0<millisecond/FSharp.Data.UnitSystems.SI.UnitNames.second>

let secondsToTimeSpan (time: float<FSharp.Data.UnitSystems.SI.UnitNames.second>): System.TimeSpan =
    System.TimeSpan.FromSeconds(float time)


let runningProcess = System.Diagnostics.Process.GetCurrentProcess()
let getUsedMemoryInMb() = float runningProcess.WorkingSet64 / 1_000_000.0
