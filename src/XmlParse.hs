{-# LANGUAGE FlexibleContexts #-}

module XmlParse where

import qualified Data.Char as Char
import Data.Conduit.Attoparsec
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.XML.Types as XML
import Text.Megaparsec
import Text.Megaparsec.Prim
import Text.Megaparsec.Pos
import XmlEvents
import qualified XmlTree as Tree

getEventLocation :: Event -> Location
getEventLocation (EventBeginElement _ _ r) = r
getEventLocation (EventEndElement _ r) = r
getEventLocation (EventContent _ r) = r
getEventLocation (EventCDATA _ r) = r

updatePosEvent :: Int -> SourcePos -> Event -> SourcePos
updatePosEvent _ p e = buildPos src startPos
  where
    (Location src pos) = getEventLocation e
    startPos = posRangeStart pos
    buildPos n (Position l c) = flip setSourceName n . flip setSourceColumn c . flip setSourceLine l $ p

tryHandle :: MonadParsec s m Event => (Event -> Either [Message] a) -> m a
tryHandle f = token updatePosEvent f

singleUnexpected :: String -> Either [Message] a
singleUnexpected = Left . pure . Unexpected

parseBegin :: Event -> Either [Message] (XML.Name, [(XML.Name, [XML.Content])], Location)
parseBegin (EventBeginElement n a p) = Right (n, a, p)
parseBegin e = singleUnexpected . show $ e

parseEnd :: XML.Name -> Event -> Either [Message] Location
parseEnd n1 (EventEndElement n2 p) | n1 == n2 = Right p
parseEnd n e = singleUnexpected $ "Expected end element " ++ show n ++ " but found " ++ show e

eventToTextContent :: Event -> Either [Message] Tree.Content
eventToTextContent (EventContent (XML.ContentText t) p) = Right (Tree.Content (Tree.ContentText t) p)
eventToTextContent (EventCDATA t p) = Right (Tree.Content (Tree.ContentText t) p)
eventToTextContent e = singleUnexpected . show $ e

eventToEntityContent :: Event -> Either [Message] Tree.Content
eventToEntityContent (EventContent (XML.ContentEntity t) p) = Right (Tree.Content (Tree.ContentEntity t) p)
eventToEntityContent e = singleUnexpected . show $ e

mergePositionRange :: PositionRange -> PositionRange -> PositionRange
mergePositionRange (PositionRange p1 p2) (PositionRange p3 p4) = PositionRange (minimum positions) (maximum positions) where positions = [p1, p2, p3, p4]

mergeLocation :: Location -> Location -> Location
mergeLocation (Location src1 p1) (Location _ p2) = Location src1 (mergePositionRange p1 p2)

getTextContentOrEmpty :: Tree.Content -> Text
getTextContentOrEmpty (Tree.Content (Tree.ContentText t) _) = t
getTextContentOrEmpty _ = Text.empty

concatContent :: MonadParsec s m Event => m Tree.Content
concatContent = do
  contents <- some (tryHandle eventToTextContent)
  case contents of
    [] -> fail "Empty content list"
    (x : xs) ->
      let text = Text.concat . fmap getTextContentOrEmpty $ contents
      in
        if Text.null text
        then fail "Empty content"
        else return $ Tree.Content (Tree.ContentText text) (foldr mergeLocation (Tree.location x) (Tree.location <$> xs))

elementOrContentParser :: MonadParsec s m Event => m (Either Tree.Element Tree.Content)
elementOrContentParser
  = (Left <$> elementParser)
  <|> (Right <$> concatContent)
  <|> (Right <$> (tryHandle eventToEntityContent))

elementParser :: MonadParsec s m Event => m Tree.Element
elementParser = do
  (name, attr, beginPos) <- tryHandle parseBegin
  children <- many elementOrContentParser
  endPos <- tryHandle (parseEnd name)
  return $ Tree.Element name attr beginPos endPos children

whitespaceContent :: MonadParsec s m Event => m ()
whitespaceContent = tryHandle whitespaceEvent
  where
    whitespaceEvent (EventContent (XML.ContentText t) _) | Text.all Char.isSpace t = Right ()
    whitespaceEvent e = singleUnexpected . show $ e

rootParser :: MonadParsec s m Event => m Tree.Element
rootParser = do
  _ <- many whitespaceContent
  elementParser

parseElementEvents :: [Event] -> Either [String] Tree.Element
parseElementEvents events = do
  case runParser rootParser "" events of
    Left e -> Left (messageString <$> errorMessages e)
    Right x -> Right x
