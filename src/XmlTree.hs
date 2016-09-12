module XmlTree where

import Data.Text (Text)
import qualified Data.XML.Types as XML
import XmlEvents (Location)

data Content = Content
  { text :: Text
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
