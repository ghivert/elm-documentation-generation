module Elm.Module.Comment exposing (..)

import Parser exposing (..)

type Struct
  = Markdown String
  | DocsTag (List String)

isEmpty : Struct -> Bool
isEmpty struct =
  case struct of
    Markdown content -> content == ""
    DocsTag content -> content == []

parse : String -> Result Error (List Struct)
parse comment =
  comment
  |> String.trim
  |> String.split "\n"
  |> List.map (run parser)
  |> List.foldr flattenResults (Ok [])

parser : Parser Struct
parser =
  oneOf
    [ symbol "@docs"
      |> andThen (always (keep oneOrMore (always True)))
      |> andThen checkEndOfLine
      |> map (String.split "," >> List.map String.trim)
      |> map DocsTag
    , succeed Markdown
      |= keep zeroOrMore (always True)
    ]

checkEndOfLine : String -> Parser String
checkEndOfLine content = map (always content) end

flattenResults
   : Result Error Struct
  -> Result Error (List Struct)
  -> Result Error (List Struct)
flattenResults element acc =
  case acc of
    Err error -> Err error
    Ok content ->
      case element of
        Err error -> Err error
        Ok element ->
          Ok (if isEmpty element then content else element :: content)
