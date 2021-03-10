{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Lib where

import Query ( groupSensesWordB, SPointer, collectRelationsSenses )
import ReadDocs ( readSuggestion, readSynset, readVote, readJL )
import UpdateSynsets ( readFromDocs, groupVotes, scoreId, applySuggestions , filterByRules, joinById) 
import FrequencyFilter ( getFrequencies, Frequency, parseFrequencies, frequencyFilter )

import Data.List ( sortBy )


processSynsets :: Integer -> FilePath -> FilePath -> FilePath -> FilePath -> IO [SPointer]
processSynsets trashold pathSyns pathSugg pathVote pathFreq =
  map fst . sortBy f <$> filtered
  where
    f x y = compare (snd y) (snd x)
    filtered = frequencyFilter trashold <$> filteredSens pathSyns pathSugg pathVote <*> filteredFreq pathFreq


filteredFreq :: FilePath -> IO [Frequency]
filteredFreq pathFreq = parseFrequencies <$> getFrequencies pathFreq


filteredSens :: [Char] -> [Char] -> [Char] -> IO [SPointer]
filteredSens pathSyns pathSugg pathVote =
  groupSensesWordB . collectRelationsSenses <$> synsets_updated
  where
    -- update synsets
    synsets = fmap readFromDocs (readJL readSynset pathSyns)
    synsets_updated = applySuggestions <$> synsets <*> suggestions_filtered
    -- collect suggestions to apply
    votes = readJL readVote pathVote
    suggestions = fmap readFromDocs (readJL readSuggestion pathSugg)
    suggestions_filtered = filterByRules 2 <$> suggestions_scores
    suggestions_scores = joinById <$> suggestions <*> suggestions_id_scores
    suggestions_id_scores = fmap (scoreId . groupVotes . readFromDocs) votes
