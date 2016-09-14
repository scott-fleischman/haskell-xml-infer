module XmlTree where

import Data.Text (Text)
import qualified Data.XML.Types as XML
import XmlEvents (Location)

data ContentItem
  = ContentText Text
  | ContentEntity Text
  deriving (Show)

data Content = Content
  { item :: ContentItem
  , location :: Location
  }
  deriving (Show)

data Element = Element
  { name :: XML.Name
  , attributes :: [(XML.Name, [XML.Content])]
  , startLocation :: Location
  , endPosition :: Location
  , children :: [Either Element Content]
  }
  deriving (Show)
