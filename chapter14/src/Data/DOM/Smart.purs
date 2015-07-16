module Data.DOM.Smart
  ( Element()
  , Attribute()
  , Content()
  , AttributeKey()
  , False()
  , True()

  , a
  , div
  , p
  , img

  , href
  , _class
  , src
  , width
  , height

  , disabled
  , checked

  , (:=)
  , emptyAttr
  , text
  , elem

  , render
  ) where

import Data.Maybe
import Data.Array (map)
import Data.String (joinWith)

newtype Element = Element
  { name         :: String
  , attribs      :: [Attribute]
  , content      :: Maybe [Content]
  }

data Content
  = TextContent String
  | ElementContent Element

newtype Attribute = Attribute
  { key          :: String
  , value        :: Maybe String
  }

element :: String -> [Attribute] -> Maybe [Content] -> Element
element name attribs content = Element
  { name:      name
  , attribs:   attribs
  , content:   content
  }

text :: String -> Content
text = TextContent

elem :: Element -> Content
elem = ElementContent

data True

data False

newtype AttributeKey a = AttributeKey String

(:=) :: AttributeKey True -> String -> Attribute
(:=) (AttributeKey key) value = Attribute
  { key: key
  , value: Just value
  }

emptyAttr :: AttributeKey False -> Attribute
emptyAttr (AttributeKey key) = Attribute { key: key, value: Nothing }

a :: [Attribute] -> [Content] -> Element
a attribs content = element "a" attribs (Just content)

div :: [Attribute] -> [Content] -> Element
div attribs content = element "div" attribs (Just content)

p :: [Attribute] -> [Content] -> Element
p attribs content = element "p" attribs (Just content)

img :: [Attribute] -> Element
img attribs = element "img" attribs Nothing

href :: AttributeKey True
href = AttributeKey "href"

_class :: AttributeKey True
_class = AttributeKey "class"

src :: AttributeKey True
src = AttributeKey "src"

width :: AttributeKey True
width = AttributeKey "width"

height :: AttributeKey True
height = AttributeKey "height"

disabled :: AttributeKey False
disabled = AttributeKey "disabled"

checked :: AttributeKey False
checked = AttributeKey "checked"

render :: Element -> String
render (Element e) =
  "<" ++ e.name ++
  " " ++ joinWith " " (map renderAttribute e.attribs) ++
  renderContent e.content

  where
  renderAttribute :: Attribute -> String
  renderAttribute (Attribute a) = a.key ++ value
    where
    value = maybe "" (\s -> "=\"" ++ s ++ "\"") a.value

  renderContent :: Maybe [Content] -> String
  renderContent Nothing = " />"
  renderContent (Just content) = 
    ">" ++ joinWith "" (map renderContentItem content) ++
    "</" ++ e.name ++ ">"
    where
    renderContentItem :: Content -> String
    renderContentItem (TextContent s) = s
    renderContentItem (ElementContent e) = render e

