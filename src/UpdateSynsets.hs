{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module UpdateSynsets where

import ReadDocs
    ( Document(_source),
      Synset(doc_id, example_pt, gloss_pt, word_pt),
      Suggestion(s_user, status, s_id, synset_id, action, params),
      Vote(suggestion_id, value) )

import Data.List ( sortBy, groupBy, sort, intercalate, sortBy, groupBy, sort )
import Data.Char ( toLower )
import Data.Maybe ( fromJust, fromMaybe )
import Data.Either ( rights )


readFromDocs :: [Either String (Document a)] -> [a]
readFromDocs = map _source . rights


groupVotes :: [Vote] -> [[Vote]]
groupVotes xs = groupBy fg (sortBy fo xs)
  where
    fg x y = suggestion_id x == suggestion_id y
    fo x y = suggestion_id x `compare` suggestion_id y


scoreId :: [[Vote]] -> [(String, Integer)]
scoreId =
  map (\x -> (suggestion x, score x)) 
  where
    suggestion = suggestion_id . head 
    score = sum . map value


joinById :: [Suggestion] -> [(String, Integer)] -> [(Suggestion, Integer)]
joinById suggestions idscores =
  _joinById [] (sort suggestions) (sort idscores)
  where
    _joinById out [] idscores = out
    _joinById out suggestions [] = [(s,0) | s <- suggestions] ++ out
    _joinById out (sg:suggestions) ((id,score):idscores) = 
      case compare (s_id sg) id of
        GT -> _joinById ((sg,0):out) suggestions idscores
        EQ -> _joinById ((sg,score):out) suggestions idscores
        LT -> _joinById out (sg:suggestions) idscores


filterByRules :: Integer -> [(Suggestion, Integer)] -> [Suggestion]
filterByRules trashold =
  map fst . filter rules
  where
    rules x = all ($ x) [rule1,rule2,rule3] 
    rule3 (s,c) = action s /= "comment"
    rule2 (s,c) = status s == "new"
    rule1 (s,c) =
      if fromMaybe "" (s_user s) `elem` users_senior
        then c >= trashold_senior
        else c >= trashold_junior
    users_senior = ["arademaker", "vcvpaiva"]
    trashold_senior = 1
    trashold_junior = trashold


groupSuggestions :: [Synset] -> [Suggestion] -> [(Synset,[Suggestion])]
groupSuggestions sns sgs =
  group_sns_sgs [] (sortBy c_sns sns) (groupBy g_sgs (sortBy c_sgs sgs))
  where
    c_sns x y = compare (doc_id x) (doc_id y)
    g_sgs x y = synset_id x == synset_id y
    c_sgs x y = compare (synset_id x) (synset_id y)
    group_sns_sgs out [] _ = out
    group_sns_sgs out ss [] = [(s,[]) | s <- ss] ++ out
    group_sns_sgs out (s:ss) (sg:sgs) =
      case compare (doc_id s) (synset_id (head sg)) of
        GT -> group_sns_sgs out (s:ss) sgs
        EQ -> group_sns_sgs ((s,sg):out) ss sgs
        LT -> group_sns_sgs ((s,[]):out) ss (sg:sgs)

applySuggestions :: [Synset] -> [Suggestion] -> [Synset]
applySuggestions synsets suggestions =
  [updateSynset ta tb | (ta, tb) <- groupSuggestions synsets suggestions]
  

applySuggestion :: Synset -> Suggestion -> Synset
applySuggestion sn sg =
  case action sg of
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


updateSynset :: Synset -> [Suggestion] -> Synset
updateSynset = foldl applySuggestion
