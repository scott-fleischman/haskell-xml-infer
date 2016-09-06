module Main where

import Options.Applicative
import XmlEvents
import XmlParse
import XmlTree

data ShowIgnored = ShowIgnored | ShowElements
data Settings = Settings
  { file :: FilePath
  , recursive :: Bool
  , showIgnored :: ShowIgnored
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

printPerLine :: (Show a) => [a] -> IO ()
printPerLine = mapM_ print

analyzeTree :: Element -> IO ()
analyzeTree (Element n _ _ _) = print n

readXml :: Settings -> IO ()
readXml (Settings path _ i) = do
  (ignored, events) <- readEvents path
  case i of
    ShowIgnored -> printPerLine ignored
    ShowElements -> case parseElementEvents path events of
      Left e -> printPerLine e
      Right x -> analyzeTree x

main :: IO ()
main = execParser opts >>= readXml
  where
  opts = info (helper <*> settings)
    ( fullDesc
    <> progDesc "Print a simple schema for XML documents"
    <> header "xml-infer - Investigate the structure of XML documents" )
