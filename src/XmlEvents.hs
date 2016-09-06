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

data Event
  = EventBeginElement XML.Name [(XML.Name, [XML.Content])] PositionRange
  | EventEndElement XML.Name PositionRange
  | EventContent XML.Content PositionRange
  | EventCDATA Text PositionRange
  deriving (Show)
instance ShowToken Event where showToken = show
instance ShowToken [Event] where showToken xs = intercalate "\n" (show <$> xs)

data Ignored
  = IgnoredBeginDocument
  | IgnoredEndDocument
  | IgnoredBeginDoctype Text (Maybe XML.ExternalID) PositionRange
  | IgnoredEndDoctype PositionRange
  | IgnoredInstruction XML.Instruction PositionRange
  | IgnoredComment Text PositionRange
  | IgnoredMissingPosition XML.Event
  deriving (Show)

splitIgnored :: (Maybe PositionRange, XML.Event) -> Either Ignored Event
splitIgnored (_, XML.EventBeginDocument) = Left IgnoredBeginDocument
splitIgnored (_, XML.EventEndDocument) = Left IgnoredEndDocument
splitIgnored (Just p, XML.EventBeginElement n as) = Right $ EventBeginElement n as p
splitIgnored (Just p, XML.EventEndElement n) = Right $ EventEndElement n p
splitIgnored (Just p, XML.EventContent c) = Right $ EventContent c p
splitIgnored (Just p, XML.EventCDATA t) = Right $ EventCDATA t p
splitIgnored (Just p, XML.EventBeginDoctype t e) = Left $ IgnoredBeginDoctype t e p
splitIgnored (Just p, XML.EventEndDoctype) = Left $ IgnoredEndDoctype p
splitIgnored (Just p, XML.EventInstruction i) = Left $ IgnoredInstruction i p
splitIgnored (Just p, XML.EventComment t) = Left $ IgnoredComment t p
splitIgnored (Nothing, e) = Left $ IgnoredMissingPosition e

splitAllIgnored :: [(Maybe PositionRange, XML.Event)] -> ([Ignored], [Event])
splitAllIgnored = foldr (aux . splitIgnored) ([], [])
  where
    aux (Left a) (as, bs) = (a : as, bs)
    aux (Right b) (as, bs) = (as, b : bs)

readEvents :: FilePath -> IO ([Ignored], [Event])
readEvents path = do
  events <- runResourceT $ sourceFile path =$= parseBytesPos def $$ sinkList
  return $ splitAllIgnored events