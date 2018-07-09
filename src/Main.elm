port module Main exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Elm.Documentation as Documentation exposing (Documentation, Alias, Union, Value)
import Elm.Documentation.Type as Type exposing (Type)
import Html.String as H

import Parser.Comment as Comment

port fromJs : (Decode.Value -> msg) -> Sub msg
port toJs : Encode.Value -> Cmd msg

type alias Model =
  Maybe (List Documentation)

type Msg
  = FromJs Decode.Value
  | ToHtmlDocumentation

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
          [ H.h1
            [ H.class "module-name" ]
            [ H.text name ]
          , H.div
            [ H.class "comments" ]
            (List.intersperse (H.div [ H.style [ ("padding", "12px") ] ] [])
              (List.concatMap (commentToHtmlString documentation) comments)
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
      [ H.pre
        [ H.class "markdown-content" ]
        [ H.text (String.join "\n" content) ]
      ]
    Comment.DocsTag content ->
      List.map (generateDocTagDocumentation documentation) content

generateDocTagDocumentation : Documentation -> String -> H.Html
generateDocTagDocumentation documentation name =
  case findByName name documentation of
    Nothing -> H.text ""
    Just content ->
      let signature = generateDocumentationWrapperDocumentation content
          comment = H.div [] [ H.text (extractDocumentationWrapperComment content) ] in
      H.div []
        [ signature
        , H.hr [] []
        , comment
        ]

generateDocumentationWrapperDocumentation : DocumentationWrapper -> H.Html
generateDocumentationWrapperDocumentation valueAliasUnion =
  case valueAliasUnion of
    DocumentationWrapperValue { name, tipe } ->
      case name of
        Documentation.Name name_ ->
          H.div
            [ H.class "function-signature" ]
            [ H.text (name_ ++ " : " ++ typeToString 2 tipe) ]
        Documentation.Op _ _ _ ->
          H.text ""
    DocumentationWrapperAlias { name, comment, args, tipe } ->
      H.div
        [ H.class "alias-signature"]
        [ H.text <|
          String.join "\n"
            [ String.join " "
              [ "type alias"
              , name
              , (String.join " " args)
              ,"="
              ]
            , typeToString 2 tipe
            ]
        ]
    DocumentationWrapperUnion { name, comment, args, tags } ->
      let
        generatedConstructors =
          String.join " | "
            (List.map
              (\(value, fieldType) ->
                String.join " "
                  [ value
                  , String.join " " (List.map (typeToString 2) fieldType)
                  ]
              )
              tags
            )
      in
        H.div
          [ H.class "union-signature" ]
          [ H.text
            <| String.append
              (String.join " "
                [ name
                , String.join " " args
                ]
              )
              (if String.length generatedConstructors == 0 then "" else " =\n")
          ]

indent : Int -> String -> String
indent space content =
  String.repeat space " " ++ content

typeToString : Int -> Type -> String
typeToString indentSpace type_ =
  case type_ of
    Type.Var value -> value
    Type.Lambda first second ->
      [ typeToString indentSpace first
      , "->"
      , typeToString indentSpace second
      ]
      |> String.join " "
    Type.Tuple values ->
      values
      |> List.map (typeToString indentSpace)
      |> String.join ", "
      |> (\content -> "(" ++ content ++ ")")
    Type.Type name types ->
      [ name
      , types
        |> List.map (typeToString indentSpace)
        |> String.join " "
      ]
      |> String.join " "
    Type.Record values maybe ->
      values
      |> List.map (\(name, fieldType) -> name ++ " : " ++ (typeToString indentSpace fieldType))
      |> String.join ", "
      |> (\content -> "{" ++ content ++ "}")

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
      case hd.name of
        Documentation.Name name_ ->
          if name == name_ then
            Just hd
          else
            findValueByName name tl
        Documentation.Op name_ _ _ ->
          if name == name_ then
            Just hd
          else
            findValueByName name tl

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
