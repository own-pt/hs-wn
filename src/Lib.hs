{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Lib where

import ReadDocs ( readSuggestion, readSynset, readVote, readJL )
import Query ( groupSensesWordB, SPointer, collectRelationsSenses )
import FrequencyFilter ( getFrequencies, Frequency, parseFrequencies, frequencyFilter )
import UpdateSynsets ( f1, f2, f3, c0, c1, c2, c4 )

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
  groupSensesWordB . collectRelationsSenses <$> (c4 <$> synsDocs <*> suggFilter)
  where
    idFilter = fmap (c0 1) scoreIds
    suggFilter = c2 <$> suggDocs <*> idFilter
    synsDocs = fmap f1 (readJL readSynset pathSyns)
    suggDocs = fmap (c1 . f1) (readJL readSuggestion pathSugg)
    scoreIds = fmap (f3 . f2 . f1) (readJL readVote pathVote)
