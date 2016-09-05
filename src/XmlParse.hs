{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}

module XmlParse where

import Control.Monad.Trans.Resource
import Conduit
import Data.Conduit.Attoparsec
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.XML.Types as XML
import Text.XML.Stream.Parse (parseBytesPos, def)
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.Combinator
import Text.Megaparsec.Pos
import Text.Megaparsec.ShowToken

data Event
  = EventBeginElement XML.Name [(XML.Name, [XML.Content])] PositionRange
  | EventEndElement XML.Name PositionRange
  | EventContent XML.Content PositionRange
  | EventCDATA Text PositionRange
  deriving (Show)
instance ShowToken Event where showToken = show
instance ShowToken [Event] where showToken xs = intercalate "\n" (show <$> xs)

data TreeText
  = TreeText Text
  | TreeEntity Text
  deriving (Show)

data Tree
  = TreeElement XML.Name [(XML.Name, [XML.Content])] (PositionRange, PositionRange) [Tree]
  | TreeContent TreeText PositionRange
  deriving (Show)

data Ignored
  = IgnoredBeginDocument
  | IgnoredEndDocument
  | IgnoredBeginDoctype Text (Maybe XML.ExternalID) PositionRange
  | IgnoredEndDoctype PositionRange
  | IgnoredInstruction XML.Instruction PositionRange
  | IgnoredComment Text PositionRange
  | IgnoredMissingPosition XML.Event
  deriving (Show)

data ShowIgnored = ShowIgnored | ShowElements
data Settings = Settings
  { file :: FilePath
  , recursive :: Bool
  , showIgnored :: ShowIgnored
  }

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

printPerLine :: (Show a) => [a] -> IO ()
printPerLine = mapM_ print

getEventPositionRange :: Event -> PositionRange
getEventPositionRange (EventBeginElement _ _ r) = r
getEventPositionRange (EventEndElement _ r) = r
getEventPositionRange (EventContent _ r) = r
getEventPositionRange (EventCDATA _ r) = r

updatePosEvent :: Int -> SourcePos -> Event -> SourcePos
updatePosEvent _ p = buildPos . posRangeEnd . getEventPositionRange
  where
    buildPos (Position l c) = flip setSourceColumn c . flip setSourceLine l $ p

tryHandle :: MonadParsec s m Event => (Event -> Either [Message] a) -> m a
tryHandle f = token updatePosEvent f

singleUnexpected :: String -> Either [Message] a
singleUnexpected = Left . pure . Unexpected

parseBegin :: Event -> Either [Message] (XML.Name, [(XML.Name, [XML.Content])], PositionRange)
parseBegin (EventBeginElement n a p) = Right (n, a, p)
parseBegin e = singleUnexpected . show $ e

parseEnd :: XML.Name -> Event -> Either [Message] PositionRange
parseEnd n1 (EventEndElement n2 p) | n1 == n2 = Right p
parseEnd n e = singleUnexpected $ "Expected end element " ++ show n ++ " but found " ++ show e

contentParser :: Event -> Either [Message] (TreeText, PositionRange)
contentParser (EventContent (XML.ContentText t) p) = Right (TreeText t, p)
contentParser (EventContent (XML.ContentEntity e) p) = Right (TreeEntity e, p)
contentParser (EventCDATA t p) = Right (TreeText t, p)
contentParser e = singleUnexpected . show $ e

elementContentParser :: MonadParsec s m Event => m Tree
elementContentParser
  = elementParser
  <|> (\(t, p) -> TreeContent t p) <$> tryHandle contentParser

elementParser :: MonadParsec s m Event => m Tree
elementParser = do
  (name, attr, beginPos) <- tryHandle parseBegin
  children <- many elementContentParser
  endPos <- tryHandle (parseEnd name)
  return $ TreeElement name attr (beginPos, endPos) children

parseElementEvents :: String -> [Event] -> IO ()
parseElementEvents sourceName events = do
  case runParser elementParser sourceName events of
    Left e -> print e
    Right x -> print x

xmlParse :: Settings -> IO ()
xmlParse (Settings path _ i) = do
  elements <- runResourceT $ sourceFile path =$= parseBytesPos def $$ sinkList
  let (ignored, events) = splitAllIgnored elements
  case i of
    ShowIgnored -> printPerLine ignored
    ShowElements -> parseElementEvents path events
