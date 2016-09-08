module XmlInfer where

import qualified Data.Char as Char
import Data.Conduit.Attoparsec (PositionRange)
import qualified Data.Text as Text
import qualified Data.XML.Types as XML

data ResultKind
  = Whitespace
  | Content
  | Element XML.Name

data Result = Result
  { kind :: ResultKind
  , position :: PositionRange
  }

toResult :: Either Element Content -> Result
toResult (Content t p) | Text.all Char.isSpace t = Result Whitespace p
toResult (Content _ p) = Result Content p
toResult (Element n _ p _ _) = Result (Element n) p
