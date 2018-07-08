port module Main exposing (..)

import Json.Decode as Decode
import Json.Encode as Encode
import Elm.Documentation as Documentation exposing (Documentation)
import Html.String as H

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
toHtmlString { name, comment, aliases, unions, values } =
  ( name
  , H.div
      [ H.style [ ("padding", "6px") ] ]
      [ H.h1
        [ H.class "module-name" ]
        [ H.text name ]
      , H.div
        [ H.class "comments" ]
        [ H.text comment ]
      ]
  )

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