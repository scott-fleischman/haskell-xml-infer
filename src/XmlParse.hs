module XmlParse where

import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.List
import Data.Text
import Data.XML.Types
import Text.XML.Stream.Parse

data OurEvent
  = OurBeginElement Name [(Name, [Content])] PositionRange
  | OurEndElement Name PositionRange
  | OurContent Content PositionRange
  | OurCDATA Text PositionRange

xmlParse :: FilePath -> IO ()
xmlParse path = do
  elements <- runResourceT $
    parseFile def path $$ consume
  print elements
