import Text.XML.Expat.SAX
import System.Environment (getArgs)
import Control.Monad (liftM)
import Data.Char (toLower)
import Data.List (sortBy)
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as C

type Freqmap = M.Map String Int
type SaxDefault = SAXEvent String String
type Tags = [String]

frequency :: (Ord a) => (b,a) -> (c,a) -> Ordering
frequency (_,a) (_,b) = compare b a

increment :: (Num a, Ord k) => k -> M.Map k a -> M.Map k a
increment = flip (M.insertWith (+)) 1

extractTags :: Tags -> Freqmap -> SaxDefault -> Freqmap
extractTags tags m (StartElement t attrs)
    | t == "word" && lookup "pos" attrs `elem` (map Just tags) = increment (map toLower word) m
    | otherwise                                                = m
    where word = case lookup "form" attrs of Just w  -> w
                                             Nothing -> error "EMPTY WORD!"
extractTags _ m _ = m

main :: IO ()
main = do
    f    <- liftM head getArgs >>= C.readFile
    tags <- liftM tail getArgs
    let stuff   = foldl (extractTags tags) M.empty (parse defaultParseOptions f)
        total   = (sortBy frequency) (M.toList stuff)
        amount  = foldl (+) 0 (map snd total)
        majBase = fromIntegral(snd . head $ total) / fromIntegral(amount)
    putStrLn $ "Number of prepositions: " ++
        show amount ++" ("++ (show $ length total) ++" unique)"
    putStrLn $ "Majority baseline ('" ++
        (fst.head $ total) ++"' occurs "++ (show.snd.head $ total) ++" times): "++ (show majBase)

