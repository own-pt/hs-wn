{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Update where

import Solr
import Data.List
import Data.Either


{- UPDATE SYNSETS -}

sourcesFromDocs :: [Either String (Document a)] -> [a]
sourcesFromDocs = map _source . rights

groupVotes :: [Vote] -> [[Vote]]
groupVotes xs = groupBy fg (sortBy fo xs)
  where
    fg x y = suggestion_id x == suggestion_id y
    fo x y = suggestion_id x `compare` suggestion_id y


getSuggestionIdScores :: [[Vote]] -> [(String, Integer)]
getSuggestionIdScores =
  map (\x -> (i x, s x)) 
  where
    i = suggestion_id . head 
    s = sum . map value


-- NOTE: filters ids with score >= treshold
filterSuggestionIdsByScore :: Integer -> [(String, Integer)] -> [String]
filterSuggestionIdsByScore th ids_score =
  [id | (id, sc) <- ids_score, sc >= th]


-- NOTE: remove comments and commited
filterSuggestionsByRules :: [Suggestion] -> [Suggestion]
filterSuggestionsByRules ss =
  filter (\s -> and $ map ($ s) [f_user, f_status, f_comment]) ss
  where
    f_user s = elem (s_user s) [Just "arademaker", Just "vcvpaiva"] 
    f_status s = status s == "new"
    f_comment s = action s /= "comment"


-- NOTE: filters suggestion with valid ids
filterSuggestions :: [Suggestion] -> [String] -> [Suggestion]
filterSuggestions ss is =
  filter_suggestions [] (sort ss) (sort is)
  where
    filter_suggestions out [] _ = out
    filter_suggestions out ss [] = out
    filter_suggestions out (s:ss) (i:is) =
      case (compare (s_id s) i) of
        LT -> filter_suggestions out ss (i:is)
        GT -> filter_suggestions out (s:ss) is
        EQ -> filter_suggestions (s:out) ss is

-- NOTE: groups suggestions for synset
groupSuggestionsBySynset :: [Synset] -> [Suggestion] -> [(Synset,[Suggestion])]
groupSuggestionsBySynset sns sgs =
  group_sns_sgs [] (sortBy c_sns sns) (groupBy g_sgs (sortBy c_sgs sgs))
  where
    c_sns x y = compare (doc_id x) (doc_id y)
    g_sgs x y = (synset_id x) == (synset_id y)
    c_sgs x y = compare (synset_id x) (synset_id y)
    group_sns_sgs out [] _ = out
    group_sns_sgs out ss [] = [(s,[]) | s <- ss] ++ out
    group_sns_sgs out (s:ss) (sg:sgs) =
      case (compare (doc_id s) (synset_id (head sg))) of
        GT -> group_sns_sgs out (s:ss) sgs
        EQ -> group_sns_sgs ((s,sg):out) ss sgs
        LT -> group_sns_sgs ((s,[]):out) ss (sg:sgs)

updateSynsets :: [Synset] -> [Suggestion] -> [Synset]
updateSynsets synsets suggestions =
  [updateSynset ta tb | (ta, tb) <- re]
  where
    re = groupSuggestionsBySynset synsets suggestions


updateSynset :: Synset -> [Suggestion] -> Synset
updateSynset sn [] = sn
updateSynset sn (sg:sgs) =
  updateSynset (applySuggestion sn sg) sgs


applySuggestion :: Synset -> Suggestion -> Synset
applySuggestion sn sg =
  case (action sg) of
    "remove-example-pt" -> sn {example_pt = removeItem (params sg) (example_pt sn)}
    "remove-gloss-pt" -> sn {gloss_pt = removeItem (params sg) (gloss_pt sn)}
    "remove-word-pt" -> sn {word_pt = removeItem (params sg) (word_pt sn)}
    "add-example-pt" -> sn {example_pt = addItem (params sg) (example_pt sn)}
    "add-gloss-pt" -> sn {gloss_pt = addItem (params sg) (gloss_pt sn)}
    "add-word-pt" -> sn {word_pt = addItem (params sg) (word_pt sn)}
    --sn _ -> sn
  where
    addItem x Nothing = Just [x]
    addItem x (Just ys) = Just (x:ys)
    removeItem x Nothing = Nothing
    removeItem x (Just ys) = Just (filter (/=x) ys)
