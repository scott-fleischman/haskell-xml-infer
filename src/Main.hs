{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

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

newtype InstanceCount = InstanceCount { getInstanceCount :: Int } deriving Num
instance Read InstanceCount where readsPrec i s = (\(a, b) -> (InstanceCount a, b)) <$> readsPrec i s

data ShowIgnored = ShowIgnored | ShowElements
data Settings = Settings
  { file :: FilePath
  , recursive :: Bool
  , showIgnored :: ShowIgnored
  , instanceCount :: InstanceCount
  }

settings :: Parser Settings
settings = Settings
  <$> strArgument
    ( metavar "PATH"
    <> help "Path to an XML file or directory of XML files"
    )
  <*> switch
    ( short 'r'
    <> long "recursive"
    <> help "Recursively search PATH for XML files"
    )
  <*> flag ShowElements ShowIgnored
    ( short 'i'
    <> long "ignored"
    <> help "Show ignored XML events"
    )
  <*> option auto
    ( short 'c'
    <> long "instance-count"
    <> value 0
    <> metavar "COUNT"
    <> help "Number of instances to show"
    )

printPerLine :: (Show a) => [a] -> IO ()
printPerLine = mapM_ print

showName :: XML.Name -> Text
showName (XML.Name ln _ Nothing) = Text.concat ["<", ln, ">"]
showName (XML.Name ln _ (Just p)) = Text.concat ["<", p, ":", ln, ">"]

textShow :: (Show a) => a -> Text
textShow = Text.pack . show

showResultKind :: ResultKind -> Text
showResultKind (XmlInfer.Element n) = showName n
showResultKind x = textShow x

showParentName :: Parent -> Text
showParentName NoParent = "-"
showParentName (Parent n) = showName n

showLocation :: Location -> Text
showLocation (Location src p) = Text.concat [Text.pack src, ":", textShow p]

printChild :: InstanceCount -> (ResultKind, [Location]) -> IO ()
printChild c (k, ps) = do
  Text.putStrLn . Text.concat $ ["  ", showResultKind k, " — ", textShow . length $ ps]
  mapM_ (Text.putStrLn . Text.append "    " . showLocation) (take (getInstanceCount c) ps)

printParent :: InstanceCount -> (Parent, ElementInfo) -> IO ()
printParent c (pn, ei) = do
  Text.putStrLn $ Text.concat [showParentName pn, " ", textShow . XmlInfer.count $ ei]
  mapM_ (printChild c) (Map.assocs . XmlInfer.instances $ ei)

analyzeTree :: InstanceCount -> Element -> IO ()
analyzeTree c e = do
  let m = infer e
  mapM_ (printParent c) (Map.assocs m)

readXml :: Settings -> IO ()
readXml (Settings path _ i c) = do
  (ignored, events) <- readEvents path
  case i of
    ShowIgnored -> printPerLine ignored
    ShowElements -> case parseElementEvents path events of
      Left e -> printPerLine e
      Right x -> analyzeTree c x

main :: IO ()
main = execParser opts >>= readXml
  where
  opts = info (helper <*> settings)
    ( fullDesc
    <> progDesc "Print a simple schema for XML documents"
    <> header "xml-infer - Investigate the structure of XML documents" )
