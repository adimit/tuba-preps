module Main where

import qualified Data.ByteString.Lazy as B
import Control.Monad (liftM)
import System.Environment (getArgs)
import Tuba.PolyReader
import Text.ParserCombinators.Poly.Lazy

main :: IO ()
main = do f <- liftM head getArgs >>= B.readFile
          putStrLn . show $ runParser (many1 sentence) (filterUseful . saxParse $ f :: [SaxEvent String])

