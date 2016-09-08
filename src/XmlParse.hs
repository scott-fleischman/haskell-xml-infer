{-# LANGUAGE FlexibleContexts #-}

module XmlParse where

import Data.Conduit.Attoparsec
import qualified Data.Text as Text
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
eventToContent (EventContent (XML.ContentText t) p) = Right (Tree.Content t p)
eventToContent (EventCDATA t p) = Right (Tree.Content t p)
eventToContent e = singleUnexpected . show $ e

mergePositionRange :: PositionRange -> PositionRange -> PositionRange
mergePositionRange (PositionRange p1 p2) (PositionRange p3 p4) = PositionRange (minimum positions) (maximum positions) where positions = [p1, p2, p3, p4]

concatContent :: MonadParsec s m Event => m Tree.Content
concatContent = do
  contents <- some (tryHandle eventToContent)
  case contents of
    [] -> fail "Empty content list"
    (x : xs) ->
      let text = Text.concat $ Tree.contentText <$> (x : xs)
      in
        if Text.null text
        then fail "Empty content"
        else return $ Tree.Content text (foldr mergePositionRange (Tree.contentPosition x) (Tree.contentPosition <$> xs))

elementOrContentParser :: MonadParsec s m Event => m (Either Tree.Element Tree.Content)
elementOrContentParser
  = (Left <$> elementParser)
  <|> (Right <$> concatContent)

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
