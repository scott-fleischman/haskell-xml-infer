module XmlTree where

import Data.Conduit.Attoparsec (PositionRange)
import Data.Text (Text)
import qualified Data.XML.Types as XML

data TreeText
  = TreeText Text
  | TreeEntity Text
  deriving (Show)

data Tree
  = TreeElement XML.Name [(XML.Name, [XML.Content])] (PositionRange, PositionRange) [Tree]
  | TreeContent TreeText PositionRange
  deriving (Show)
