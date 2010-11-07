module Main where

import Format
import Data.Maybe (fromMaybe)
import Data.List
import Text.XML.Expat.SAX
import qualified Data.ByteString.Lazy as C

import System.Environment (getArgs)
import Control.Monad (liftM)

grabSentences :: C.ByteString -> [Sentence String]
grabSentences f = collectSentences $ parse defaultParseOptions f

collectSentences :: [SAXEvent String String] -> [Sentence String]
collectSentences events = (Sentence $ generateSentence (tail sent)):(collectSentences rest)
      where (sent,rest) = span (stillA "sentence") (dropWhile (notA "sentence") events)

notA :: String -> SAXEvent String String -> Bool
notA s (StartElement tag _) | tag == s = False
notA _ _                               = True 
stillA s (EndElement tag)   | tag == s = False
stillA _ _                             = True

generateSentence :: [SAXEvent String String] -> [Node String]
generateSentence ((StartElement tag attrs):es) 
        | tag == "node"  = [Node category (generateSentence es)]
        | tag == "word"  = (Word surface morphology):(generateSentence es)
        where category   = fromMaybe (error "Node without category")
                             (lookup "cat" attrs)
              surface    = fromMaybe (error "Word without surface")
                             (lookup "form" attrs)
              morphology = fromMaybe (error "Word without morpholoy")
                            (lookup "morph" attrs)
generateSentence (e:es) = generateSentence es
generateSentence s = error $ "EOL?" ++ show s

main = do
    f <- liftM head getArgs >>= C.readFile
    putStrLn . show $ grabSentences f
