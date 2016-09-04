module Main where

import Options.Applicative
import System.FilePath
import XmlParse

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
  <*> flag ShowExisting ShowIgnored
    ( short 'i'
    <> long "ignored"
    <> help "Show ignored XML events"
    )

main :: IO ()
main = execParser opts >>= xmlParse
  where
  opts = info (helper <*> settings)
    ( fullDesc
    <> progDesc "Print a simple schema for XML documents"
    <> header "xml-infer - Investigate the structure of XML documents" )
