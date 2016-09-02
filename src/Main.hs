module Main where

import Options.Applicative
import System.FilePath
import XmlParse

data Settings = Settings
  { file :: FilePath
  , recursive :: Bool
  }

settings :: Parser Settings
settings = Settings
  <$> strOption
    ( short 'p'
    <> long "path"
    <> metavar "PATH"
    <> help "Path to an XML file or directory of XML files"
    )
  <*> switch
    ( short 'r'
    <> long "recursive"
    <> help "Recursively search PATH for XML files"
    )

go :: Settings -> IO ()
go (Settings p r) = xmlParse p

main :: IO ()
main = execParser opts >>= go
  where
  opts = info (helper <*> settings)
    ( fullDesc
    <> progDesc "Print a simple schema for XML documents"
    <> header "xml-infer - Investigate the structure of XML documents" )
