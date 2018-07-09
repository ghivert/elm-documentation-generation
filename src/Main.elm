port module Main exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Elm.Documentation as Documentation exposing (Documentation, Alias, Union, Value)
import Elm.Documentation.Type as Type exposing (Type)
import Html.String as H

import Parser.Comment as Comment
import Helpers.Html as Helpers
import Helpers.String as Helpers
import Generator.Style as Style
port fromJs : (Decode.Value -> msg) -> Sub msg
port toJs : Encode.Value -> Cmd msg

type alias Model =
  Maybe (List Documentation)

type Msg
  = FromJs Decode.Value
  | ToHtmlDocumentation

pageScript : String
pageScript = """
document.addEventListener('DOMContentLoaded', function(event) {
  const converter = new showdown.Converter({ extensions: [ 'prettify' ] })
  const htmlNodes = document.getElementsByClassName('markdown')
  const nodes = Array.prototype.slice.call(htmlNodes)
  nodes.forEach(function(elem) {
    const md = converter.makeHtml(elem.textContent)
    elem.innerHTML = md
  })
})
"""

main : Program Never Model Msg
main =
  Platform.program
    { init = init
    , update = update
    , subscriptions = subscriptions
    }

init : (Model, Cmd Msg)
init =
  (Nothing, Cmd.none)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    FromJs value ->
      value
      |> Decode.decodeValue (Decode.list Documentation.decoder)
      |> Result.toMaybe
      |> update ToHtmlDocumentation
    ToHtmlDocumentation ->
      case model of
        Nothing -> (model, Cmd.none)
        Just documentations ->
          ( model
          , documentations
            |> List.map toHtmlString
            |> List.map toJsonDocsFiles
            |> encodeDocsFilesToJsonMsg
            |> toJs
          )

subscriptions : Model -> Sub Msg
subscriptions model =
  fromJs FromJs

toHtmlString : Documentation -> (String, H.Html)
toHtmlString ({ name, comment } as documentation) =
  case Comment.parse comment of
    Err error -> (toString error, H.text "error")
    Ok comments ->
      ( name
      , H.div
          [ H.style [ ("padding", "6px") ] ]
          [ H.node "style" [] [ H.text Style.githubStyle ]
          , H.node "style" [] [ H.text Style.pageStyle ]
          , H.node "script" [] [ H.text pageScript ]
          , H.node "link"
            [ H.href "https://fonts.googleapis.com/css?family=Open+Sans|Roboto+Mono"
            , H.rel "stylesheet"
            ] []
          , H.node "script"
            [ H.src "https://cdn.rawgit.com/showdownjs/showdown/1.8.6/dist/showdown.min.js" ] []
          , H.node "script"
            [ H.src "https://cdn.jsdelivr.net/npm/showdown-prettify@1.3.0/dist/showdown-prettify.min.js" ] []
          , H.node "script"
            [ H.src "https://cdn.rawgit.com/google/code-prettify/master/loader/run_prettify.js" ] []
          , H.h1
            [ H.class "module-name" ]
            [ H.text name ]
          , H.div
            [ H.class "comments" ]
            (comments
             |> List.concatMap (commentToHtmlString documentation)
             |> List.intersperse Helpers.spacer
            )
          ]
      )

type DocumentationWrapper
  = DocumentationWrapperValue Value
  | DocumentationWrapperAlias Alias
  | DocumentationWrapperUnion Union

extractDocumentationWrapperComment : DocumentationWrapper -> String
extractDocumentationWrapperComment documentation =
  case documentation of
    DocumentationWrapperValue content -> content.comment
    DocumentationWrapperAlias content -> content.comment
    DocumentationWrapperUnion content -> content.comment

commentToHtmlString : Documentation -> Comment.Comment -> List H.Html
commentToHtmlString documentation comment =
  case comment of
    Comment.Markdown content ->
      [ H.div
        [ H.class "markdown" ]
        [ H.text (Helpers.newlineJoin content) ]
      ]
    Comment.DocsTag content ->
      List.map (generateDocTagDocumentation documentation) content

generateDocTagDocumentation : Documentation -> String -> H.Html
generateDocTagDocumentation documentation name =
  case findByName name documentation of
    Nothing -> H.text ""
    Just content ->
      let bodyComment = String.trim (extractDocumentationWrapperComment content) in
      H.div
        [ H.class "function-definition" ]
        (List.append
          [ generateDocumentationWrapperDocumentation content ]
          (if String.length bodyComment == 0 then
            []
           else
             [ H.hr [] []
             , H.div
               [ H.class "markdown" ]
               [ H.text bodyComment ]
             ]
          )
        )

generateDocumentationWrapperDocumentation : DocumentationWrapper -> H.Html
generateDocumentationWrapperDocumentation valueAliasUnion =
  case valueAliasUnion of
    DocumentationWrapperValue value -> generateValueDocumentation value
    DocumentationWrapperAlias value -> generateAliasDocumentation value
    DocumentationWrapperUnion value -> generateUnionDocumentation value

generateValueDocumentation : Documentation.Value -> H.Html
generateValueDocumentation { name, tipe } =
  case name of
    Documentation.Name name_ ->
      H.div
        [ H.class "function-signature" ]
        [ H.text (name_ ++ " : " ++ typeToString 2 tipe) ]
    Documentation.Op _ _ _ ->
      H.text ""

generateAliasDocumentation : Documentation.Alias -> H.Html
generateAliasDocumentation { name, comment, args, tipe } =
  let arguments = Helpers.spaceJoin args in
  H.div
    [ H.class "alias-signature"]
    [ H.text <|
      Helpers.newlineJoin
        [ Helpers.spaceJoin
          [ "type alias", name, arguments, "=" ]
        , typeToString 2 tipe
        ]
    ]

generateUnionDocumentation : Documentation.Union -> H.Html
generateUnionDocumentation { name, comment, args, tags } =
  let
    generatedConstructors =
      tags
      |> List.map generateUnionConstructors
      |> List.intersperse "|"
      |> Helpers.spaceJoin

    unionSignature =
      generatedConstructors
      |> addEqualAndNewlineIfNotNull
      |> String.append
        (Helpers.spaceJoin [ name, Helpers.spaceJoin args ])
  in
    H.div
      [ H.class "union-signature" ]
      [ H.text unionSignature ]

addEqualAndNewlineIfNotNull : String -> String
addEqualAndNewlineIfNotNull constructors =
  if String.length constructors == 0 then
    ""
  else
    " =\n" ++ (Helpers.indent 2 constructors)

generateUnionConstructors : ( String, List Type ) -> String
generateUnionConstructors (value, fieldType) =
  Helpers.spaceJoin
    [ value
    , fieldType
      |> List.map (typeToString 2)
      |> Helpers.spaceJoin
    ]

typeToString : Int -> Type -> String
typeToString indentSpace type_ =
  case type_ of
    Type.Var value -> value
    Type.Lambda first second ->
      [ first, second ]
      |> List.map (typeToString indentSpace)
      |> List.intersperse "->"
      |> Helpers.spaceJoin
    Type.Tuple values ->
      values
      |> List.map (typeToString indentSpace)
      |> Helpers.commaJoin
      |> Helpers.surroundByParens
    Type.Type name types ->
      [ name
      , types
        |> List.map (typeToString indentSpace)
        |> Helpers.spaceJoin
      ]
      |> List.filter ((/=) "")
      |> Helpers.spaceJoin
    Type.Record values maybe ->
      values
      |> List.map (generateRecordFields indentSpace)
      |> Helpers.commaJoin
      |> Helpers.surroundByBraces

generateRecordFields : Int -> (String, Type) -> String
generateRecordFields indentSpace (name, fieldType) =
  Helpers.spaceJoin
    [ name, ":", (typeToString indentSpace fieldType) ]

findByName : String -> Documentation -> Maybe DocumentationWrapper
findByName name { aliases, unions, values } =
  case findAliasByName name aliases of
    Just content -> Just (DocumentationWrapperAlias content)
    Nothing ->
      case findUnionByName name unions of
        Just content -> Just (DocumentationWrapperUnion content)
        Nothing ->
          case findValueByName name values of
            Just content -> Just (DocumentationWrapperValue content)
            Nothing -> Nothing

findAliasByName : String -> List Alias -> Maybe Alias
findAliasByName name aliases =
  case aliases of
    [] -> Nothing
    hd :: tl ->
      if name == hd.name then
        Just hd
      else
        findAliasByName name tl

findUnionByName : String -> List Union -> Maybe Union
findUnionByName name unions =
  case unions of
    [] -> Nothing
    hd :: tl ->
      if name == hd.name then
        Just hd
      else
        findUnionByName name tl

findValueByName : String -> List Value -> Maybe Value
findValueByName name values =
  case values of
    [] -> Nothing
    hd :: tl ->
      let
        okOrNext name_ =
          if name == name_ then
            Just hd
          else
            findValueByName name tl
      in
        case hd.name of
          Documentation.Name name_ ->
            okOrNext name_
          Documentation.Op name_ _ _ ->
            okOrNext name_

toJsonDocsFiles : (String, H.Html) -> Encode.Value
toJsonDocsFiles (name, content) =
  Encode.object
    [ ( name
      , Encode.string
        (H.htmlToString content)
      )
    ]

encodeDocsFilesToJsonMsg : List Encode.Value -> Encode.Value
encodeDocsFilesToJsonMsg values =
  let
    msgEncoder content =
      Encode.object
        [ ("msg", Encode.string "CreateDocsFiles")
        , ("docsFiles", content)
        ]
  in
    values
    |> Encode.list
    |> msgEncoder
