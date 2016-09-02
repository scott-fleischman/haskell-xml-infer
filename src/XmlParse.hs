module XmlParse where

import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.List
import Text.XML.Stream.Parse

xmlParse :: FilePath -> IO ()
xmlParse path = do
  elements <- runResourceT $
    parseFile def path $$ consume
  print elements
