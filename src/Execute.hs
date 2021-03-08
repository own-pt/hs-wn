{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Execute where

import Solr
import Query
import Filter
import Update

import Data.List


processSynsets :: Integer -> FilePath -> FilePath -> FilePath -> FilePath -> IO [SPointer]
processSynsets trashold path_syns path_sugg path_vote path_freq =
  (map fst . sortBy f) <$> filteredSensePointersByFrequency
  where
    f x y = compare (snd y) (snd x)
    frequencies = getFrequencies path_freq
    filteredSensePointers = filterSensePointers path_syns path_sugg path_vote
    filteredSensePointersByFrequency = filterByFrequency trashold <$> filteredSensePointers <*> frequencies
    

filterSensePointers :: [Char] -> [Char] -> [Char] -> IO [SPointer]
filterSensePointers path_syns path_sugg path_vote =
  (groupSensesWordB . collectSensePointers) <$> (updateSynsets <$> synsets <*> filtered_suggestions)
  where
    synsets = fmap (sourcesFromDocs) (readJL readSynset path_syns)
    -- filtered suggestions
    scored_suggestion_ids = fmap (getSuggestionIdScores . groupVotes . sourcesFromDocs) (readJL readVote path_vote)
    filtered_suggestion_ids = fmap (filterSuggestionIdsByScore 1) scored_suggestion_ids
    filtered_suggestions_by_rules = fmap (filterSuggestionsByRules . sourcesFromDocs)  (readJL readSuggestion path_sugg)
    filtered_suggestions = filterSuggestions <$> filtered_suggestions_by_rules <*> filtered_suggestion_ids
