module Main where

import Options.Applicative
import System.FilePath
import XmlParse

data Settings = Settings
  { file :: FilePath
  , recursive :: Bool
  , showIgnored :: Bool
  }

toShowIgnored :: Bool -> ShowIgnored
toShowIgnored True = ShowIgnored
toShowIgnored False = ShowExisting

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
  <*> switch
    ( short 'i'
    <> long "ignored"
    <> help "Show ignored XML events"
    )

go :: Settings -> IO ()
go (Settings p r i) = xmlParse p (toShowIgnored i)

main :: IO ()
main = execParser opts >>= go
  where
  opts = info (helper <*> settings)
    ( fullDesc
    <> progDesc "Print a simple schema for XML documents"
    <> header "xml-infer - Investigate the structure of XML documents" )
