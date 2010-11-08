{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Data.Char (toLower)
import Data.List (sortBy)
import Tuba.Format
import Tuba.PolyReader
import Text.ParserCombinators.Poly.Lazy
import Text.XML.Expat.SAX
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as C

import System.Console.CmdArgs

data MajBaseConfig = M { inputFile :: FilePath
                       , surfaceForms :: [String]
                       , posTags :: [String]
                       } deriving (Show, Data, Typeable)

baseline :: MajBaseConfig
baseline = M { inputFile = def
               &= help "XML file with the TübaDZ Corpus" &= typFile
             , surfaceForms = def
               &= help "Surface forms to match (if any.)" &= typ "[STRING]"
             , posTags = ["APPR", "APPRART", "APPO", "APZR", "PROP"]
               &= help "PoS tags to match (if any.)" &= typ "[PoSTag]" }
             &= help "Gives a majority baseline for a TübaDZ XML corpus"
             &= summary "MajorityBaseline .01"

-- polymorphic to accomodate for different string types.
type Freqmap t = M.Map t Int
type Tags    t = [t]

frequency :: (Ord a) => (b,a) -> (c,a) -> Ordering
frequency (_,a) (_,b) = compare b a 

increment :: (Num a, Ord k) => k -> M.Map k a -> M.Map k a
increment = flip (M.insertWith (+)) 1

nodes :: (GenericXMLString t, Ord t) => (Node t -> Bool) -> Freqmap t -> Node t -> Freqmap t
nodes p m w@(Word f _ _ ) = if p w then increment (gxFromString $ map toLower (gxToString f)) m 
                                   else m
nodes p m (Node _ _ ns) = foldl (nodes p) m ns

sentences :: (GenericXMLString t, Ord t) => (Node t -> Bool) -> Freqmap t -> Sentence t -> Freqmap t
sentences p m (Sentence _ ns) = foldl (nodes p) m ns

corpus :: (GenericXMLString t, Ord t) => (Node t -> Bool) -> [Sentence t] -> Freqmap t
corpus p ss = foldl (sentences p) M.empty ss

match :: (GenericXMLString t) => MajBaseConfig -> (Node t -> Bool)
match config = match'
    where match' (Word s _m p) =    s `elem` (map gxFromString (surfaceForms config))
                                 || p `elem` (map gxFromString (posTags config))
          match' _             = False

main :: IO ()
main = do config <- cmdArgs $ baseline &= program "majb"
          f <- C.readFile $ inputFile config
          let freqmap = corpus (match config) p
              p = fst $ runParser (many1 sentence) (filterUseful . saxParse $ f :: [SaxEvent String])
              total = sortBy frequency (M.toList freqmap)
              amount = sum (map snd total)
              majBase = (fromIntegral (snd.head $ total) / fromIntegral amount :: Double)
          putStrLn $ "Number of prepositions: " ++
             show amount ++" ("++ show (length total) ++" unique)"
          putStrLn $ "Majority baseline ('" ++
             (fst.head $ total) ++"' occurs "++ (show.snd.head $ total) ++" times): "++ show majBase
