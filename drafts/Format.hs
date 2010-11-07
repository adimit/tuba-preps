module Format where

data Sentence s = Sentence [Node s] deriving Show

data Node s = Word { surface    :: s
                   , morphology :: String }
            | Node { category   :: String
                   , children   :: [Node s] } deriving Show
