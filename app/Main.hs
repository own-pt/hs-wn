module Main where

import Solr
import Query
import Filter
import Update
import Data.List

main :: IO ()
main = do
  out <- showOut <$> process 20
  putStrLn out
  where
    showOut [] = ""
    showOut ((rel,_):rels) = (wordA rel) ++"\t"++ (intercalate "/" $ wordB rel) ++ "\n" ++ (showOut rels)

process trashold  =
  sortBy (\x y -> compare (snd y) (snd x)) <$> (frequencyFilter trashold <$> relationsSyns <*> filteredFreq)
relationsSyns = collectRelationsSenses <$> filteredSyns
filteredFreq = (parseFrequencies) <$> (getFrequencies "/home/fredson/wn/dhbb/frequencies")
filteredSyns =
  c4 <$> sy_doc <*> sg_filter
  where
    id_filter = fmap (c0 1) id_scores
    sg_filter = c2 <$> sg_doc <*> id_filter
    sy_doc = fmap (f1) (readJL readSynset "/home/fredson/wn/dump/wn.json")
    sg_doc = fmap (c1 . f1) (readJL readSuggestion "/home/fredson/wn/dump/suggestion.json")
    id_scores = fmap (f3 . f2 . f1) (readJL readVote "/home/fredson/wn/dump/votes.json")
