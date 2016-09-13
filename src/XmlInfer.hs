module XmlInfer where

import qualified Data.Char as Char
import qualified Data.Either as Either
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.XML.Types as XML
import qualified XmlTree as Tree
import XmlEvents (Location)

data Child
  = Attribute XML.Name
  | Element XML.Name
  | Content
  | Whitespace
  deriving (Eq, Ord, Show)

data Ancestors = Ancestors { getAncestors :: [XML.Name] }
  deriving (Eq, Ord, Show)

addAncestor :: XML.Name -> Ancestors -> Ancestors
addAncestor n (Ancestors ns) = Ancestors (n : ns)

data ElementInfo = ElementInfo
  { locations :: [Location]
  , ancestors :: Map Ancestors [Location]
  , childInstances :: Map Child [Location]
  , childSets :: Map (Set Child) [Location]
  }
  deriving (Show)

emptyAncestors :: Ancestors
emptyAncestors = Ancestors []

emptyElementInfo :: ElementInfo
emptyElementInfo = ElementInfo [] Map.empty Map.empty Map.empty

contentChild :: Tree.Content -> (Child, Location)
contentChild (Tree.Content t l) | Text.all Char.isSpace t = (Whitespace, l)
contentChild (Tree.Content _ l) = (Content, l)

bodyChild :: Either Tree.Element Tree.Content -> (Child, Location)
bodyChild (Left (Tree.Element n _ l _ _)) = (Element n, l)
bodyChild (Right c) = contentChild c

immediateChildren :: Tree.Element -> [(Child, Location)]
immediateChildren (Tree.Element _ as l _ cs) = attributeChildren ++ bodyChildren
  where
    attributeChildren = fmap (\x -> (Attribute x, l)) . fmap fst $ as
    bodyChildren = fmap bodyChild cs

elementChildSet :: Tree.Element -> Set Child
elementChildSet = Set.fromList . fmap fst . immediateChildren

updateOrAddValue :: (Ord k) => (v -> w -> w) -> (v -> w) -> k -> v -> Map k w -> Map k w
updateOrAddValue f g k v m = case Map.lookup k m of
  Just vs -> Map.insert k (f v vs) m
  Nothing -> Map.insert k (g v) m

mapConsValue :: (Ord k) => k -> v -> Map k [v] -> Map k [v]
mapConsValue = updateOrAddValue (:) pure

addSelfInfo :: Ancestors -> Tree.Element -> ElementInfo -> ElementInfo
addSelfInfo anc e@(Tree.Element _ _ l _ _) (ElementInfo ls ancMap chInstMap chSetMap) =
  ElementInfo
  { locations = (l : ls)
  , ancestors = mapConsValue anc l ancMap
  , childInstances = foldr (\(ch, loc) m -> mapConsValue ch loc m) chInstMap (immediateChildren e)
  , childSets = mapConsValue (elementChildSet e) l chSetMap
  }

mapAddSelfInfo :: Ancestors -> XML.Name -> Tree.Element -> Map XML.Name ElementInfo -> Map XML.Name ElementInfo
mapAddSelfInfo a = updateOrAddValue (addSelfInfo a) (\x -> addSelfInfo a x emptyElementInfo)

addElementInfo :: Ancestors -> Tree.Element -> Map XML.Name ElementInfo -> Map XML.Name ElementInfo
addElementInfo a e@(Tree.Element n _ _ _ cs) = addChildrenInfo . mapAddSelfInfo a n e
  where
    a' = addAncestor n a
    cs' = Either.lefts cs
    addChildrenInfo x = foldr (addElementInfo a') x cs'

infer :: Tree.Element -> Map XML.Name ElementInfo
infer e = addElementInfo emptyAncestors e Map.empty
