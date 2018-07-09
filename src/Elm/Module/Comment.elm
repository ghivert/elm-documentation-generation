module Elm.Module.Comment exposing (..)

import Parser exposing (..)

type alias Comment =
  { header : String
  , docs : List String
  }

type Struct
  = Header String
  | Docs String
  | End

-- parse : String -> Result Error Comment
-- parse comment =
--   comment
--   |> String.split "\n"
--   |> run
--
-- parser : Parser Comment
-- parser =
--   succeed Comment
--   |= keep zeroOrMore isNotArobase
--   |=

t : Parser (List Struct)
t =
  oneOf
    [ keep oneOrMore isNotArobase
      |> andThen
        (\header ->
          let deb = Debug.log "header" header in
          oneOf
            [ succeed (List.append [ Header header ] << List.singleton << Docs)
              |. symbol "@docs"
              |= keep oneOrMore (not << isNewline)
            , keep (Exactly 1) ((==) '@')
              |> andThen (\at ->
                keep zeroOrMore (not << isNewline)
                |> map (\endOfLine -> [ Header <| header ++ at ++ endOfLine ])
              )
            , end |> map (always (Debug.log "end" ([ End ])))
            ]
        )
    ]
  |> repeat zeroOrMore
  |> map List.concat
  |> andThen (\content -> end |> map (always (Debug.log "ici" content)))

isNotArobase : Char -> Bool
isNotArobase char = char /= '@'

isNewline : Char -> Bool
isNewline char = char == '\n'
