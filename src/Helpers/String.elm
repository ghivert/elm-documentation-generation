module Helpers.String exposing (..)

indent : Int -> String -> String
indent space content =
  String.repeat space " " ++ content

surroundByParens : String -> String
surroundByParens content = "(" ++ content ++ ")"

surroundByBraces : String -> String
surroundByBraces content = "{" ++ content ++ "}"

spaceJoin : List String -> String
spaceJoin = String.join " "

commaJoin : List String -> String
commaJoin = String.join ", "

newlineJoin : List String -> String
newlineJoin = String.join "\n"
