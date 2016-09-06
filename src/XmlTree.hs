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

getEntities :: Element -> [(Text, PositionRange)]
getEntities (Element _ _ _ xs) = foldr go [] xs
  where
    go (Left (Element _ _ _ zs)) ys = foldr go [] zs ++ ys
    go (Right (ContentText _ _)) ys = ys
    go (Right (ContentEntity t p)) ys = (t, p) : ys
