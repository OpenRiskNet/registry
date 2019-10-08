module Orn.Registry.Client.ReactMarkdown

open Fable.Import
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Core.JsInterop

// See https://github.com/rexxars/react-markdown for other available options

type ReactMarkdownProp =
    | Source of string
    | LinkTarget of string

let inline reactMarkdown (properties : ReactMarkdownProp list) : ReactElement =
    ofImport "default" "react-markdown" (keyValueList Fable.Core.CaseRules.LowerFirst properties) []
