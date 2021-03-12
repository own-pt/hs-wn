{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Lib where

import Query ( SPointer, collectRelationsSenses )
import ReadDocs ( readSuggestion, readSynset, readVote, readJL )
import UpdateSynsets ( readFromDocs, groupVotes, scoreId, applySuggestions , filterByRules, joinById)

import Data.List ( sortBy )




updatedSPointers :: [Char] -> [Char] -> [Char] -> IO [SPointer]
updatedSPointers pathSyns pathSugg pathVote =
  collectRelationsSenses <$> synsets_updated
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
