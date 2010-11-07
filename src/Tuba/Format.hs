module Tuba.Format where

-- | Format declarations for the TübaDZ format.
-- This documents the data format used throughout the library.

type Attrs t = [(t,t)]

type Corpus t = [Sentence t]

data Sentence t = Sentence { sHeadline  :: Bool
                           , sContents  :: [Node t]
                           }
                  deriving Show

data Node t = Word { wSurface      :: t
                   , wMorphology   :: t
                   , wPartofSpeech :: t
                   }
            | Node { nCategory   :: t
                   , nFunction   :: t
                   , nChildren   :: [Node t]
                   }
              deriving (Show, Ord, Eq)
