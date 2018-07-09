module Helpers.Html exposing (..)

import Html.String as H

spacer : H.Html
spacer =
  H.div
    [ H.style [ ("padding", "12px") ] ]
    []
