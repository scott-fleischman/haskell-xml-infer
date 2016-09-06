{-# LANGUAGE FlexibleContexts #-}

module XmlParse where

import Data.Conduit.Attoparsec
import qualified Data.XML.Types as XML
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.Pos
import XmlEvents
import qualified XmlTree as Tree

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

eventToContent :: Event -> Either [Message] Tree.Content
eventToContent (EventContent (XML.ContentText t) p) = Right (Tree.ContentText t p)
eventToContent (EventContent (XML.ContentEntity e) p) = Right (Tree.ContentEntity e p)
eventToContent (EventCDATA t p) = Right (Tree.ContentText t p)
eventToContent e = singleUnexpected . show $ e

elementOrContentParser :: MonadParsec s m Event => m (Either Tree.Element Tree.Content)
elementOrContentParser
  = (Left <$> elementParser)
  <|> (Right <$> tryHandle eventToContent)

elementParser :: MonadParsec s m Event => m Tree.Element
elementParser = do
  (name, attr, beginPos) <- tryHandle parseBegin
  children <- many elementOrContentParser
  endPos <- tryHandle (parseEnd name)
  return $ Tree.Element name attr (beginPos, endPos) children

parseElementEvents :: String -> [Event] -> Either [String] Tree.Element
parseElementEvents source events = do
  case runParser elementParser source events of
    Left e -> Left (messageString <$> errorMessages e)
    Right x -> Right x
