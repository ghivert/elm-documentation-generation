module Elm.Module.Comment exposing (Struct(..), parse)

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
    [ succeed DocsTag
      |. keyword "@docs"
      |. ignore (Exactly 1) isWhitespace
      |= repeat oneOrMore docsTagParser
    , succeed Markdown
      |= keep zeroOrMore (always True)
    ]

docsTagParser : Parser String
docsTagParser =
  let keepVariableName = keep oneOrMore isNotComma in
  oneOf
    [ keepVariableName |> andThen checkEndOfLine
    , keepVariableName |> andThen ignoreCommaAndWhitespace
    ]

checkEndOfLine : String -> Parser String
checkEndOfLine content = map (always content) end

isComma : Char -> Bool
isComma char = char == ','

isNotComma : Char -> Bool
isNotComma = not << isComma

isWhitespace : Char -> Bool
isWhitespace char = char == ' '

ignoreCommaAndWhitespace : String -> Parser String
ignoreCommaAndWhitespace content =
  symbol ","
  |> andThen (always spaces)
  |> andThen (always (succeed content))

spaces : Parser ()
spaces = ignore zeroOrMore isWhitespace

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
