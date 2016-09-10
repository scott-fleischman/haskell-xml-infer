module XmlInfer where

import qualified Data.Char as Char
import Data.Conduit.Attoparsec (PositionRange)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.XML.Types as XML
import qualified XmlTree as Tree

data ResultKind
  = Whitespace
  | Content
  | Element XML.Name
  deriving (Eq, Ord, Show)

data Result = Result
  { kind :: ResultKind
  , position :: PositionRange
  }
  deriving (Show)

contentToResult :: Tree.Content -> Result
contentToResult (Tree.Content t p) | Text.all Char.isSpace t = Result Whitespace p
contentToResult (Tree.Content _ p) = Result Content p

addResult
  :: XML.Name
  -> Result
  -> Map XML.Name (Map ResultKind [PositionRange])
  -> Map XML.Name (Map ResultKind [PositionRange])
addResult n (Result k p) m = case Map.lookup n m of
  Just mk -> Map.insert n kindMap m where
    kindMap = case Map.lookup k mk of
      Just ps -> Map.insert k (p : ps) mk
      Nothing -> Map.insert k [p] mk
  Nothing -> Map.insert n (Map.insert k [p] Map.empty) m

addChildResult
  :: XML.Name
  -> Either Tree.Element Tree.Content
  -> Map XML.Name (Map ResultKind [PositionRange])
  -> Map XML.Name (Map ResultKind [PositionRange])
addChildResult n (Left (Tree.Element n2 _ p _ cs)) m = foldr (addChildResult n2) (addResult n (Result (Element n2) p) m) cs
addChildResult n (Right c@(Tree.Content _ _)) m = addResult n (contentToResult c) m

infer :: Tree.Element -> Map XML.Name (Map ResultKind [PositionRange])
infer (Tree.Element n _ _ _ cs) = foldr (addChildResult n) Map.empty cs
