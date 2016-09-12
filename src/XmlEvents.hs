{-# LANGUAGE FlexibleInstances #-}

module XmlEvents where

import Control.Monad.Trans.Resource
import Conduit
import Data.Conduit.Attoparsec (PositionRange)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.XML.Types as XML
import Text.Megaparsec.ShowToken (ShowToken, showToken)
import Text.XML.Stream.Parse (parseBytesPos, def)

data Location = Location
  { filePath :: FilePath
  , position :: PositionRange
  }
  deriving (Eq, Ord, Show)

data Event
  = EventBeginElement XML.Name [(XML.Name, [XML.Content])] Location
  | EventEndElement XML.Name Location
  | EventContent XML.Content Location
  | EventCDATA Text Location
  deriving (Show)
instance ShowToken Event where showToken = show
instance ShowToken [Event] where showToken xs = intercalate "\n" (show <$> xs)

data Ignored
  = IgnoredBeginDocument FilePath
  | IgnoredEndDocument FilePath
  | IgnoredBeginDoctype Text (Maybe XML.ExternalID) Location
  | IgnoredEndDoctype Location
  | IgnoredInstruction XML.Instruction Location
  | IgnoredComment Text Location
  | IgnoredMissingPosition FilePath XML.Event
  deriving (Show)

loc :: FilePath -> PositionRange -> Location
loc = Location

splitIgnored :: FilePath -> (Maybe PositionRange, XML.Event) -> Either Ignored Event
splitIgnored src (_, XML.EventBeginDocument) = Left $ IgnoredBeginDocument src
splitIgnored src (_, XML.EventEndDocument) = Left $ IgnoredEndDocument src
splitIgnored src (Just p, XML.EventBeginElement n as) = Right $ EventBeginElement n as (loc src p)
splitIgnored src (Just p, XML.EventEndElement n) = Right $ EventEndElement n (loc src p)
splitIgnored src (Just p, XML.EventContent c) = Right $ EventContent c (loc src p)
splitIgnored src (Just p, XML.EventCDATA t) = Right $ EventCDATA t (loc src p)
splitIgnored src (Just p, XML.EventBeginDoctype t e) = Left $ IgnoredBeginDoctype t e (loc src p)
splitIgnored src (Just p, XML.EventEndDoctype) = Left $ IgnoredEndDoctype (loc src p)
splitIgnored src (Just p, XML.EventInstruction i) = Left $ IgnoredInstruction i (loc src p)
splitIgnored src (Just p, XML.EventComment t) = Left $ IgnoredComment t (loc src p)
splitIgnored src (Nothing, e) = Left $ IgnoredMissingPosition src e

splitAllIgnored :: FilePath -> [(Maybe PositionRange, XML.Event)] -> ([Ignored], [Event])
splitAllIgnored src = foldr (aux . splitIgnored src) ([], [])
  where
    aux (Left a) (as, bs) = (a : as, bs)
    aux (Right b) (as, bs) = (as, b : bs)

readEvents :: FilePath -> IO ([Ignored], [Event])
readEvents path = do
  events <- runResourceT $ sourceFile path =$= parseBytesPos def $$ sinkList
  return $ splitAllIgnored path events
