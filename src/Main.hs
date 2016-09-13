{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.XML.Types as XML
import Options.Applicative
import qualified System.FilePath.Find as Find
import XmlEvents
import XmlInfer
import XmlParse
--import XmlTree

data ShowIgnored = ShowIgnored | ShowElements
data SortSetting = SortByElementName | SortByAncestor
data Settings = Settings
  { file :: FilePath
  , glob :: String
  , showIgnored :: ShowIgnored
  , sortSetting :: SortSetting
  , locationCount :: Int
  , ancestorCount :: Int
  , childInstanceCount :: Int
  , childSetCount :: Int
  }

newtype Indent = Indent { getIndent :: Int }

settings :: Parser Settings
settings = Settings
  <$> strArgument
    ( metavar "PATH"
    <> help "Path to an XML file or directory of XML files"
    )
  <*> strOption
    ( long "glob"
    <> short 'g'
    <> value ""
    <> metavar "GLOB"
    <> help "Glob pattern to match files"
    )
  <*> flag ShowElements ShowIgnored
    ( long "show-ignored"
    <> help "Show ignored XML events"
    )
  <*> flag SortByElementName SortByAncestor
    ( long "sort-by-ancestor"
    <> help "Sort by ancestor"
    )
  <*> option auto
    ( long "locations"
    <> short 'l'
    <> value 0
    <> metavar "COUNT"
    <> help "Number of locations to show"
    )
  <*> option auto
    ( long "ancestors"
    <> short 'a'
    <> value 0
    <> metavar "COUNT"
    <> help "Number of ancestors to show"
    )
  <*> option auto
    ( long "child-instances"
    <> short 'i'
    <> value 0
    <> metavar "COUNT"
    <> help "Number of child instances to show"
    )
  <*> option auto
    ( long "child-sets"
    <> short 's'
    <> value 0
    <> metavar "COUNT"
    <> help "Number of child sets to show"
    )

printPerLine :: (Show a) => [a] -> IO ()
printPerLine = mapM_ print

noIndent :: Indent
noIndent = Indent 0

increaseIndent :: Indent -> Indent
increaseIndent (Indent i) = Indent (i + 1)

singleIndent :: Indent
singleIndent = increaseIndent noIndent

showIndent :: Indent -> Text
showIndent (Indent i) = Text.replicate i "  "

showName :: XML.Name -> Text
showName (XML.Name ln _ Nothing) = ln
showName (XML.Name ln _ (Just p)) = Text.concat [p, ":", ln]

textShow :: (Show a) => a -> Text
textShow = Text.pack . show

showElementName :: XML.Name -> Text
showElementName x = Text.concat ["<", showName x, ">"]

showChild :: Child -> Text
showChild (XmlInfer.Element n) = showElementName n
showChild (XmlInfer.Attribute n) = Text.concat [showName n, "=\"…\""]
showChild x = textShow x

showLocation :: Location -> Text
showLocation (Location src p) = Text.concat [Text.pack src, ":", textShow p]

showAncestors :: Ancestors -> Text
showAncestors (Ancestors []) = "(root)"
showAncestors (Ancestors ns@(_ : _)) = Text.concat . List.intersperse " " . fmap showElementName . reverse $ ns

showChildSet :: Set Child -> Text
showChildSet s =
  if Set.null s
  then "(empty)"
  else Text.concat . List.intersperse ", " . fmap showChild . Set.toList $ s

printWithIndent :: Indent -> Text -> IO ()
printWithIndent i t = Text.putStrLn $ Text.concat [showIndent i, t]

printLocationsInfo :: Int -> Indent -> Text -> [Location] -> IO ()
printLocationsInfo ct indent label ls = do
  printWithIndent indent $ Text.concat [label, ": ", textShow . length $ ls]
  mapM_ (printWithIndent (increaseIndent indent) . showLocation) (take ct ls)

printMapInfo :: (Ord k) => Int -> Text -> (k -> Text) -> Map k [Location] -> IO ()
printMapInfo ct label showKey m = do
  printWithIndent singleIndent $ Text.concat [label, ":"]
  mapM_ (\(k, ls) -> printLocationsInfo ct (increaseIndent singleIndent) (showKey k) ls) (Map.assocs m)

printElementInfo :: Settings -> (XML.Name, ElementInfo) -> IO ()
printElementInfo s (n, i) = do
  Text.putStrLn $ showElementName n
  printLocationsInfo (locationCount s) singleIndent "locations" (locations i)
  printMapInfo (ancestorCount s) "ancestors" showAncestors (ancestors i)
  printMapInfo (childInstanceCount s) "child instances" showChild (childInstances i)
  printMapInfo (childSetCount s) "child sets" showChildSet (childSets i)

printAnalysis :: Settings -> Map XML.Name ElementInfo -> IO ()
printAnalysis s m = do
  let
    getSortKey (n, i) = case sortSetting s of
      SortByAncestor -> (length . Map.keys . ancestors $ i, Map.keys . ancestors $ i, n)
      SortByElementName -> (0, [], n)
  let orderedPairs = List.sortOn getSortKey (Map.assocs m)
  mapM_ (printElementInfo s) orderedPairs

splitEithers :: [Either [a] b] -> Either [a] [b]
splitEithers = foldr go (Right [])
  where
    go (Left xs) (Right _) = Left xs
    go (Left xs) (Left xs') = Left (xs ++ xs')
    go (Right _) (Left xs) = Left xs
    go (Right y) (Right ys) = Right (y : ys)

parseAnalyze :: Settings -> [[Event]] -> IO ()
parseAnalyze s ess = do
  let ps = splitEithers . fmap parseElementEvents $ ess
  case ps of
    Left es -> printPerLine es
    Right xs -> printAnalysis s . foldr mergeElementMap Map.empty . fmap infer $ xs

readXml :: Settings -> IO ()
readXml s = do
  let
    match = case glob s of
      [] -> Find.always
      (_ : _) -> Find.fileName Find.~~? glob s
  names <- Find.find Find.always match (file s)
  allEventPairs <- mapM readEvents names
  case showIgnored s of
    ShowIgnored -> printPerLine . List.concat . fmap fst $ allEventPairs
    ShowElements -> parseAnalyze s . fmap snd $ allEventPairs

main :: IO ()
main = execParser opts >>= readXml
  where
  opts = info (helper <*> settings)
    ( fullDesc
    <> progDesc "Print a simple schema for XML documents"
    <> header "xml-infer - Investigate the structure of XML documents" )
