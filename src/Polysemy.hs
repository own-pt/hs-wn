module Polysemy where

import Data.Map ( fromListWith )
import Data.Maybe ( mapMaybe )
import ReadDocs ( Synset(word_pt) )


getPolysemias synsets =
  fromListWith (+) [(sense, 1) | sense <- words]
  where
    words = concat $ mapMaybe word_pt synsets