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
  , showIgnored :: ShowIgnored
  , instanceCount :: InstanceCount
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
    ( short 'c'
    <> long "child-instances"
    <> value 0
    <> metavar "COUNT"
    <> help "Number of child instances to show"
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

printElementInfo :: (XML.Name, ElementInfo) -> IO ()
printElementInfo (n, i) = do
  Text.putStrLn $ showElementName n
  Text.putStrLn $ Text.concat ["  locations: ", textShow . length . locations $ i]
  Text.putStrLn $ Text.concat ["  ancestors: ", textShow . Map.keys . ancestors $ i]
  Text.putStrLn $ Text.concat ["  child instances: ", textShow . Map.keys . childInstances $ i]
  Text.putStrLn $ Text.concat ["  child sets: ", textShow . Map.keys . childSets $ i]

analyzeTree :: Settings -> Element -> IO ()
analyzeTree _ e = do
  let m = infer e
  mapM_ printElementInfo (Map.assocs m)

readXml :: Settings -> IO ()
readXml s@(Settings path i _) = do
  (ignored, events) <- readEvents path
  case i of
    ShowIgnored -> printPerLine ignored
    ShowElements -> case parseElementEvents path events of
      Left e -> printPerLine e
      Right x -> analyzeTree s x

main :: IO ()
main = execParser opts >>= readXml
  where
  opts = info (helper <*> settings)
    ( fullDesc
    <> progDesc "Print a simple schema for XML documents"
    <> header "xml-infer - Investigate the structure of XML documents" )
