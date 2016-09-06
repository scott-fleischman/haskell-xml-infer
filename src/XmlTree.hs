module XmlTree where

import Data.Conduit.Attoparsec (PositionRange)
import Data.Text (Text)
import qualified Data.XML.Types as XML

data Content
  = ContentText Text PositionRange
  | ContentEntity Text PositionRange
  deriving (Show)

data Element = Element
  { name :: XML.Name
  , attributes :: [(XML.Name, [XML.Content])]
  , positionStart :: (PositionRange, PositionRange)
  , children :: [Either Element Content]
  }
  deriving (Show)
