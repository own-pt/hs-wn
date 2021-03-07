{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Update where

import Solr
import Data.List
import Data.Either

{- UPDATE SYNSETS -}

f1 :: [Either String (Document a)] -> [a]
f1 = map _source . rights

f2 :: [Vote] -> [[Vote]]
f2 xs = groupBy fg (sortBy fo xs)
  where
    fg = \x y -> suggestion_id x == suggestion_id y
    fo = \x y -> suggestion_id x `compare` suggestion_id y

-- NOTE: lists (suggestion id, Score)
f3 :: [[Vote]] -> [(String, Integer)]
f3 =
  map (\x -> (i x, s x)) 
  where
    i = suggestion_id . head 
    s = sum . map value

-- NOTE: simple acception/rejection rule
-- NOTE: filters ids with score >= treshold
c0 :: Integer -> [(String, Integer)] -> [String]
c0 th ids_score =
  [id | (id, sc) <- ids_score, sc >= th]

-- NOTE: remove comments and commited
c1 :: [Suggestion] -> [Suggestion]
c1 ss =
  filter (\s -> and $ map ($ s) [f_user, f_status, f_comment]) ss
  where
    f_user s = elem (s_user s) [Just "arademaker", Just "vcvpaiva"] 
    f_status s = status s == "new"
    f_comment s = action s /= "comment"

-- NOTE: filters suggestion with valid ids
c2 :: [Suggestion] -> [String] -> [Suggestion]
c2 ss is =
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
-- c3 :: [Synset] -> [Suggestion] -> [(Synset,[Suggestion])]
-- c3 sns sgs =
--   [(sn, zips sn sgs) | sn <- sns]
--   where
--     zips sn sgs = filter (\sg -> grps sn sg) sgs
--     grps sn sg = (doc_id sn) == (synset_id sg)
c3 :: [Synset] -> [Suggestion] -> [(Synset,[Suggestion])]
c3 sns sgs =
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

c4 :: [Synset] -> [Suggestion] -> [Synset]
c4 synsets suggestions =
  [updateSynset ta tb | (ta, tb) <- re]
  where
    re = c3 synsets suggestions

-- NOTE: compose
-- f = fmap (f1) (readJL readSynset "/home/fredson/openWordnet-PT/dump/wn.json")
f =
  c4 <$> sy_doc <*> sg_filter
  where
    id_filter = fmap (c0 1) id_scores
    sg_filter = c2 <$> sg_doc <*> id_filter
    sy_doc = fmap (f1) (readJL readSynset "/home/fredson/openWordnet-PT/dump/wn.json")
    sg_doc = fmap (c1 . f1) (readJL readSuggestion "/home/fredson/openWordnet-PT/dump/suggestion.json")
    id_scores = fmap (f3 . f2 . f1) (readJL readVote "/home/fredson/openWordnet-PT/dump/votes.json")

-- NOTE: main rules funcction
-- NOTE: found actions using: cat path/to/suggestion.json | grep -oP "(?<=(\"action\":))\"[a-z-]+\"" | sort | uniq

-- NOTE: discuss optimal solution
applySuggestion :: Synset -> Suggestion -> Synset
applySuggestion sn sg =
  case (action sg) of
    "remove-example-pt" -> sn {example_pt = removeItem (params sg) (example_pt sn)} --ok
    "remove-gloss-pt" -> sn {gloss_pt = removeItem (params sg) (gloss_pt sn)} --ok
    "remove-word-pt" -> sn {word_pt = removeItem (params sg) (word_pt sn)} --ok
    "add-example-pt" -> sn {example_pt = addItem (params sg) (example_pt sn)} --ok
    "add-gloss-pt" -> sn {gloss_pt = addItem (params sg) (gloss_pt sn)} --ok
    "add-word-pt" -> sn {word_pt = addItem (params sg) (word_pt sn)} --ok
    --sn _ -> sn
  where
    addItem x Nothing = Just [x]
    addItem x (Just ys) = Just (x:ys)
    removeItem x Nothing = Nothing
    removeItem x (Just ys) = Just (filter (/=x) ys)

updateSynset :: Synset -> [Suggestion] -> Synset
updateSynset sn [] = sn
updateSynset sn (sg:sgs) =
  updateSynset (applySuggestion sn sg) sgs

-- TODO
-- c0 :: Integer -> [(String, Integer)] -> [(Suggestions, Integer)]
-- c1 :: [Suggestion, Inteter] -> [Suggestion->Bool] -> [Suggestion]

-- on removeElement, when list is [], put Nothing
