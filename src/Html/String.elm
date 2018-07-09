module Html.String exposing (..)

type Html
  = Html
    { tag : String
    , attributes : List Attribute
    , children : List (Html)
    }
  | Text String

type Attribute
  = StringAttribute (String, String)
  | IntAttribute (String, Int)
  | StyleAttribute (List (String, String))

node : String -> List Attribute -> List Html -> Html
node tag attributes children =
  Html
    { tag = tag
    , attributes = attributes
    , children = children
    }

text : String -> Html
text content = Text content

attribute : String -> String -> Attribute
attribute name content = StringAttribute (name, content)

div : List Attribute -> List Html -> Html
div = node "div"

a : List Attribute -> List Html -> Html
a = node "a"

code : List Attribute -> List Html -> Html
code = node "code"

pre : List Attribute -> List Html -> Html
pre = node "pre"

span : List Attribute -> List Html -> Html
span = node "span"

br : List Attribute -> List Html -> Html
br = node "br"

hr : List Attribute -> List Html -> Html
hr = node "hr"

h1 : List Attribute -> List Html -> Html
h1 = node "h1"

class : String -> Attribute
class = attribute "class"

style : List (String, String) -> Attribute
style content = StyleAttribute content

href : String -> Attribute
href = attribute "href"

rel : String -> Attribute
rel = attribute "rel"

src : String -> Attribute
src = attribute "src"

htmlToString : Html -> String
htmlToString html =
  case html of
    Text content -> content
    Html { tag, attributes, children } ->
      children
      |> List.map htmlToString
      |> String.join ""
      |> openingAndClosingTags tag attributes

openingAndClosingTags : String -> List Attribute -> String -> String
openingAndClosingTags tagName attributes content =
  let stringifiedAttributes = attributes
                              |> List.map attributeToString
                              |> String.join " "
      openingTag = [ "<" ++ tagName
                   , stringifiedAttributes ++ ">"
                   ]
                   |> String.join (if List.length attributes > 0 then " " else "") in
   openingTag ++ content ++ "</" ++ tagName ++ ">"

attributeToString : Attribute -> String
attributeToString attribute =
  case attribute of
    StringAttribute (name, content) -> name ++ "=" ++ "\"" ++ content ++ "\""
    IntAttribute (name, content) -> name ++ "=" ++ toString content
    StyleAttribute content -> "style=\"" ++ String.join "" (List.map styleToString content) ++ "\""

styleToString : (String, String) -> String
styleToString (name, value) =
  name ++ ":" ++ value ++ ";"
