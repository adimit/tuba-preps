data Sentence s = Sentence [Node s]

data Node s = Word { surface    :: s
                   , pos        :: String }
            | Node { category   :: String
                   , children   :: [Node s] }


