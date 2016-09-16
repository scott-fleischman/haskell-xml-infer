{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.List as List
import Data.Monoid
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
import qualified System.Directory as Dir
import XmlEvents
import XmlInfer
import XmlParse

data ShowIgnored = ShowIgnored | ShowElements
data ShowErrors = ShowErrors | SkipErrors
data SortSetting = SortByElementName | SortByAncestor
data Verbose = Verbose | NotVerbose
data Settings = Settings
  { getFilePath :: FilePath
  , getInclude :: String
  , getExclude :: String
  , getVerbose :: Verbose
  , getShowIgnored :: ShowIgnored
  , getShowErrors :: ShowErrors
  , getSortSetting :: SortSetting
  , getLocationCount :: Int
  , getAncestorCount :: Int
  , getChildInstanceCount :: Int
  , getChildSetCount :: Int
  }

newtype Indent = Indent { getIndent :: Int }

settings :: Parser Settings
settings = Settings
  <$> strArgument
    ( metavar "PATH"
    <> help "Path to an XML file or directory of XML files"
    )
  <*> strOption
    ( long "include"
    <> short 'i'
    <> value ""
    <> metavar "INCLUDE"
    <> help "Include pattern to match files"
    )
  <*> strOption
    ( long "exclude"
    <> short 'e'
    <> value ""
    <> metavar "EXCLUDE"
    <> help "Exclude pattern to match files"
    )
  <*> flag NotVerbose Verbose
    ( long "verbose"
    <> short 'v'
    <> help "Show verbose output"
    )
  <*> flag ShowElements ShowIgnored
    ( long "show-ignored"
    <> help "Show ignored XML events"
    )
  <*> flag SkipErrors ShowErrors
    ( long "show-errors"
    <> help "Show error details for invalid XML files"
    )
  <*> flag SortByAncestor SortByElementName
    ( long "sort-by-element"
    <> help "Sort by element name"
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
    <> short 'c'
    <> value 0
    <> metavar "COUNT"
    <> help "Number of child instances to show"
    )
  <*> option auto
    ( long "child-sets"
    <> short 'C'
    <> value 0
    <> metavar "COUNT"
    <> help "Number of child sets to show"
    )

isVerbose :: Verbose -> Bool
isVerbose Verbose = True
isVerbose NotVerbose = False

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

printSummary :: Indent -> Text -> Summary -> IO ()
printSummary indent label s = do
  printWithIndent indent $ Text.concat [label, ": ", textShow . locationCount $ s]
  mapM_ (printWithIndent (increaseIndent indent) . showLocation) (initialLocations s)

printMapInfo :: (Ord k) => Text -> (k -> Text) -> Map k Summary -> IO ()
printMapInfo label showKey m = do
  printWithIndent singleIndent $ Text.concat [label, ":"]
  mapM_ (\(k, ls) -> printSummary (increaseIndent singleIndent) (showKey k) ls) (Map.assocs m)

printElementInfo :: (XML.Name, ElementSummary) -> IO ()
printElementInfo (n, i) = do
  Text.putStrLn $ showElementName n
  printSummary singleIndent "locations" (locations i)
  printMapInfo "ancestors" showAncestors (ancestors i)
  printMapInfo "child instances" showChild (childInstances i)
  printMapInfo "child sets" showChildSet (childSets i)

printAnalysis :: Settings -> Map XML.Name ElementSummary -> IO ()
printAnalysis s m = do
  let
    getSortKey (n, i) = case getSortSetting s of
      SortByAncestor -> (length . Map.keys . ancestors $ i, Map.keys . ancestors $ i, n)
      SortByElementName -> (0, [], n)
  let orderedPairs = List.sortOn getSortKey (Map.assocs m)
  mapM_ printElementInfo orderedPairs

data Result = Result
  { resultIgnored :: Map FilePath [Ignored]
  , resultErrors :: Map FilePath [String]
  , resultInference :: Map XML.Name ElementSummary
  }

emptyResult :: Result
emptyResult = Result mempty mempty mempty

appendResult :: SummaryLimit -> Result -> Result -> Result
appendResult s (Result x1 y1 z1) (Result x2 y2 z2) = Result (mappend x1 x2) (mappend y1 y2) (mergeElementMap s z1 z2)

getLimits :: Settings -> SummaryLimit
getLimits s = SummaryLimit
  { limitLocations = getLocationCount s
  , limitAncestors = getAncestorCount s
  , limitChildInstances = getChildInstanceCount s
  , limitChildSets = getChildSetCount s
  }

getResult :: Settings -> FilePath -> IO Result
getResult s p = do
  when (isVerbose . getVerbose $ s) . printWithIndent singleIndent . Text.pack $ p 
  (ignored, events) <- readEvents p
  let smap = Map.singleton p
  let
    (errors, inference) = case parseElementEvents events of
      Left es -> (smap es, mempty)
      Right x -> (mempty, infer (getLimits s) x)
  return $ Result (smap ignored) errors inference

getGlobNames :: Settings -> IO [FilePath]
getGlobNames s = do
  let includeInput = getInclude s
  let include = if null includeInput then "*.xml" else includeInput
  let includeMatch = Find.fileName Find.~~? include

  let exclude = getExclude s
  let excludeMatch = if null exclude then Find.always else Find.fileName Find./~? exclude
  let match = includeMatch Find.&&? excludeMatch

  names <- Find.find Find.always match (getFilePath s)
  return names

getNames :: Settings -> IO [FilePath]
getNames s = do
  let path = getFilePath s
  fileExists <- Dir.doesFileExist path
  dirExists <- Dir.doesDirectoryExist path

  if fileExists then
    return [path]
  else if dirExists then
    getGlobNames s
  else do
    Text.putStrLn "Path doesn't exist"
    return []

readXml :: Settings -> IO ()
readXml s = do
  names <- getNames s
  Text.putStrLn $ Text.concat ["Loading ", textShow . length $ names, " files…"]

  (Result ignored errors inference) <- fmap (foldr (appendResult (getLimits s)) emptyResult) . mapM (getResult s) $ names

  let allIgnored = List.filter isShowable . mconcat . Map.elems $ ignored
  when (not . null $ allIgnored) $ do
    Text.putStrLn $ Text.concat ["Ignored ", textShow . length $ allIgnored, " events in ", textShow . length . Map.keys $ ignored ," files"]

  let errorFilePaths = Map.keys errors
  let hasErrors = not . null $ errorFilePaths 
  when hasErrors $ do
    Text.putStrLn $ Text.concat ["Errors in ", textShow . length $ errorFilePaths, " files"]
    mapM_ (printWithIndent singleIndent . Text.pack) errorFilePaths

  case getShowIgnored s of
    ShowIgnored -> printPerLine allIgnored
    ShowElements -> do
      case (hasErrors, getShowErrors s) of
        (True, ShowErrors) -> printPerLine $ Map.elems errors
        _ -> printAnalysis s inference

main :: IO ()
main = execParser opts >>= readXml
  where
  opts = info (helper <*> settings)
    ( fullDesc
    <> progDesc "Print a simple schema for XML documents"
    <> header "xml-infer - Investigate the structure of XML documents" )
