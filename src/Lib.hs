{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Lib where

import Query ( groupSensesWordB, SPointer, collectRelationsSenses )
import ReadDocs ( readSuggestion, readSynset, readVote, readJL )
import UpdateSynsets ( readFromDocs, groupVotes, scoreId, applySuggestions , filterByRules, joinById) 
import FrequencyFilter ( getFrequencies, Frequency, parseFrequencies, frequencyFilter )

import Data.List ( sortBy )


processSynsets :: Integer -> FilePath -> FilePath -> FilePath -> FilePath -> IO [SPointer]
processSynsets trashold pathSyns pathSugg pathVote pathFreq =
  map fst . sortBy f <$> spointers_filtered
  where
    f x y = compare (snd y) (snd x)
    frequencies = parseFrequencies <$> getFrequencies pathFreq
    spointers_updated = updatedSens pathSyns pathSugg pathVote
    spointers_filtered = frequencyFilter trashold <$> spointers_updated <*> frequencies


updatedSens :: [Char] -> [Char] -> [Char] -> IO [SPointer]
updatedSens pathSyns pathSugg pathVote =
  groupSensesWordB . collectRelationsSenses <$> synsets_updated
  where
    -- update synsets
    synsets = fmap readFromDocs (readJL readSynset pathSyns)
    synsets_updated = applySuggestions <$> synsets <*> suggestions_filtered
    -- collect suggestions to apply
    votes = readJL readVote pathVote
    suggestions = fmap readFromDocs (readJL readSuggestion pathSugg)
    suggestions_filtered = filterByRules <$> suggestions_scores
    suggestions_scores = joinById <$> suggestions <*> suggestions_id_scores
    suggestions_id_scores = fmap (scoreId . groupVotes . readFromDocs) votes
