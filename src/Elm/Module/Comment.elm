module Elm.Module.Comment exposing (Struct(..), parse)

import Parser exposing (..)

type InternalStruct
  = InternalMarkdown String
  | InternalDocsTag (List String)

type Struct
  = Markdown (List String)
  | DocsTag (List String)

isEmpty : InternalStruct -> Bool
isEmpty struct =
  case struct of
    InternalMarkdown content -> content == ""
    InternalDocsTag content -> content == []

parse : String -> Result Error (List Struct)
parse comment =
  comment
  |> String.trim
  |> String.split "\n"
  |> List.map (run parser)
  |> List.foldr flattenResults (Ok [])

parser : Parser InternalStruct
parser =
  oneOf
    [ succeed InternalDocsTag
      |. keyword "@docs"
      |. ignore (Exactly 1) isWhitespace
      |= repeat oneOrMore docsTagParser
    , succeed InternalMarkdown
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
   : Result Error InternalStruct
  -> Result Error (List Struct)
  -> Result Error (List Struct)
flattenResults element acc =
  case acc of
    Err error -> Err error
    Ok content ->
      case element of
        Err error -> Err error
        Ok element ->
          Ok <|
            case element of
              InternalMarkdown markdownContent ->
                case content of
                  hd :: tl ->
                    case hd of
                      Markdown content_ ->
                        Markdown (markdownContent :: content_) :: tl
                      _ ->
                        Markdown [ markdownContent ] :: hd :: tl
                  [] ->
                    [ Markdown [ markdownContent ] ]
              InternalDocsTag docsTagContent ->
                DocsTag docsTagContent :: content
