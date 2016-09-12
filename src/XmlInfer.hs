module XmlInfer where

import qualified Data.Char as Char
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.XML.Types as XML
import qualified XmlTree as Tree
import XmlEvents (Location)

data ResultKind
  = Whitespace
  | Content
  | Element XML.Name
  | Attribute XML.Name
  deriving (Eq, Ord, Show)

data ElementInfo = ElementInfo
  { count :: Int
  , instances :: Map ResultKind [Location]
  }

data Parent
  = NoParent
  | Parent XML.Name
  deriving (Eq, Ord, Show)

type TreeInfo = Map Parent ElementInfo

addResult
  :: Parent
  -> ResultKind
  -> Location
  -> TreeInfo
  -> TreeInfo
addResult pn k p m = case Map.lookup pn m of
  Just (ElementInfo c mk) -> Map.insert pn kindMap m where
    kindMap = case Map.lookup k mk of
      Just ps -> ElementInfo c (Map.insert k (p : ps) mk)
      Nothing -> ElementInfo c (Map.insert k [p] mk)
  Nothing -> Map.insert pn (ElementInfo 0 (Map.insert k [p] Map.empty)) m

addChildResult
  :: Parent
  -> Either Tree.Element Tree.Content
  -> TreeInfo
  -> TreeInfo
addChildResult n (Left e) m = addElement n e m
addChildResult n (Right (Tree.Content t p)) m | Text.all Char.isSpace t = addResult n Whitespace p m
addChildResult n (Right (Tree.Content _ p)) m = addResult n Content p m

incrementElementCount
  :: Parent
  -> TreeInfo
  -> TreeInfo
incrementElementCount pn m = case Map.lookup pn m of
  Just (ElementInfo c mk) -> Map.insert pn (ElementInfo (c + 1) mk) m
  Nothing -> Map.insert pn (ElementInfo 1 Map.empty) m

addElement
  :: Parent
  -> Tree.Element
  -> TreeInfo
  -> TreeInfo
addElement pn (Tree.Element n as p _ cs) m = addedAttributes
  where
    selfParent = Parent n
    addedSelfCount = incrementElementCount selfParent m
    addedAsChild = addResult pn (Element n) p addedSelfCount
    addedChildren = foldr (addChildResult selfParent) addedAsChild cs
    addedAttributes = foldr (\an -> addResult selfParent (Attribute an) p) addedChildren (fst <$> as)

infer :: Tree.Element -> TreeInfo
infer e = addElement NoParent e Map.empty
