{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import qualified Data.Map.Strict as Map
import qualified Data.XML.Types as XML
import Options.Applicative
import XmlEvents
import XmlInfer
import XmlParse
import XmlTree

data ShowIgnored = ShowIgnored | ShowElements
data Settings = Settings
  { file :: FilePath
  , showIgnored :: ShowIgnored
  , locationCount :: Int
  , ancestorCount :: Int
  , childInstanceCount :: Int
  , childSetCount :: Int
  }

settings :: Parser Settings
settings = Settings
  <$> strArgument
    ( metavar "PATH"
    <> help "Path to an XML file or directory of XML files"
    )
  <*> flag ShowElements ShowIgnored
    ( long "show-ignored"
    <> help "Show ignored XML events"
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

printAncestors :: Ancestors -> [Location] -> IO ()
printAncestors a ls = do
  Text.putStrLn $ Text.concat ["    ", showAncestors a, ": ", textShow . length $ ls]

printChildInstance :: Child -> [Location] -> IO ()
printChildInstance c ls = do
  Text.putStrLn $ Text.concat ["    ", showChild c, ": ", textShow . length $ ls]

showChildSet :: Set Child -> Text
showChildSet s =
  if Set.null s
  then "(empty)"
  else Text.concat . List.intersperse ", " . fmap showChild . Set.toList $ s

printChildSet :: Set Child -> [Location] -> IO ()
printChildSet s ls = do
  Text.putStrLn $ Text.concat ["    ", showChildSet s, ": ", textShow . length $ ls]

printLocationsInfo :: Int -> Text -> Text -> [Location] -> IO ()
printLocationsInfo ct indent label ls = do
  Text.putStrLn $ Text.concat [indent, label, ": ", textShow . length $ ls]
  mapM_ (\l -> Text.putStrLn $ Text.concat [indent, "  ", showLocation l]) (take ct ls)

printElementInfo :: Settings -> (XML.Name, ElementInfo) -> IO ()
printElementInfo s (n, i) = do
  Text.putStrLn $ showElementName n
  printLocationsInfo (locationCount s) "  " "locations" (locations i)

  Text.putStrLn "  ancestors:"
  mapM_ (uncurry printAncestors) (Map.assocs . ancestors $ i)

  Text.putStrLn "  child instances:"
  mapM_ (uncurry printChildInstance) (Map.assocs . childInstances $ i)

  Text.putStrLn "  child sets:"
  mapM_ (uncurry printChildSet) (Map.assocs . childSets $ i)

analyzeTree :: Settings -> Element -> IO ()
analyzeTree s e = do
  let m = infer e
  mapM_ (printElementInfo s) (Map.assocs m)

readXml :: Settings -> IO ()
readXml s = do
  (ignored, events) <- readEvents (file s)
  case showIgnored s of
    ShowIgnored -> printPerLine ignored
    ShowElements -> case parseElementEvents (file s) events of
      Left e -> printPerLine e
      Right x -> analyzeTree s x

main :: IO ()
main = execParser opts >>= readXml
  where
  opts = info (helper <*> settings)
    ( fullDesc
    <> progDesc "Print a simple schema for XML documents"
    <> header "xml-infer - Investigate the structure of XML documents" )
