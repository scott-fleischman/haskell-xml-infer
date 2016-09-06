{-# LANGUAGE FlexibleContexts #-}

module XmlParse where

import Data.Conduit.Attoparsec
import Data.Text (Text)
import qualified Data.XML.Types as XML
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.Combinator
import Text.Megaparsec.Pos
import Text.Megaparsec.ShowToken
import XmlEvents
import XmlTree

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

parseElementEvents :: String -> [Event] -> Either [String] Tree
parseElementEvents sourceName events = do
  case runParser elementParser sourceName events of
    Left e -> Left (messageString <$> errorMessages e)
    Right x -> Right x
