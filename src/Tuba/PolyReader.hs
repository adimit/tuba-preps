module Tuba.PolyReader where

import Text.ParserCombinators.Poly.Lazy
import Tuba.Format
import qualified Text.XML.Expat.SAX as X
import qualified Data.ByteString.Lazy as B

type SaxParser t a = Parser (SaxEvent t) a
type SaxEvent t = X.SAXEvent t t -- ^ We don't need to be that flexible

saxParse :: (X.GenericXMLString t) => B.ByteString -> [SaxEvent t]
saxParse = X.parse X.defaultParseOptions

filterUseful :: (X.GenericXMLString t) => [SaxEvent t] -> [SaxEvent t]
filterUseful = filter (\e -> startEnd "sentence" e || startEnd "word" e || startEnd "node" e)
    where startEnd s (X.StartElement t _) = X.gxFromString s == t
          startEnd s (X.EndElement t) = X.gxFromString s == t
          startEnd _ _ = False

cdata :: (X.GenericXMLString t) => SaxParser t t
cdata = satR f
    where f (X.CharacterData s) = Just s
          f _                   = Nothing

lookup' :: (X.GenericXMLString t) => Attrs t -> String -> t
lookup' attrs s = case lookup (X.gxFromString s) attrs of
                       Just b  -> b
                       Nothing -> X.gxFromString "NOTHING!"

startTag :: (X.GenericXMLString t) => String -> SaxEvent t -> Maybe (Attrs t)
startTag t (X.StartElement t' attrs) | (X.gxFromString t) == t' = Just attrs
startTag _ _ = Nothing

endTag :: (X.GenericXMLString t) => String -> SaxEvent t -> Bool
endTag t (X.EndElement t') = (X.gxFromString t) == t'
endTag _ _ = False

satR :: (X.GenericXMLString t) => (SaxEvent t -> Maybe a) -> SaxParser t a
satR p = do x <- next
            case p x of
                 (Just a)  -> return a
                 (Nothing) -> fail "Can't get no satisfaction!"

headlineTag :: (X.GenericXMLString t) => t
headlineTag = X.gxFromString "%% HEADLINE"

sentence :: (X.GenericXMLString t) => SaxParser t (Sentence t)
sentence = do attrs <- satR (startTag "sentence")
              nodes <- many1 $ oneOf [word, node]
              _     <- satisfy (endTag "sentence")
              return $ Sentence (lookup' attrs "commend" == headlineTag) nodes

node, word :: (X.GenericXMLString t) => SaxParser t (Node t)
word = do attrs <- satR (startTag "word")
          _     <- satisfy (endTag "word")
          let l = lookup' attrs
          return $ Word (l "form") (l "morph") (l "pos")

node = do attrs <- satR (startTag "node")
          nodes <- many1 $ oneOf [word,node]
          _     <- satisfy (endTag "node")
          let l = lookup' attrs
          return $ Node (l "cat") (l "func") nodes
