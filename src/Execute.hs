{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Execute where

import Solr
import Query
import Filter
import Update

import Data.List

-- NOTE: filters and sorts by frequency and returns the SensePointers
processSynsets :: Integer -> FilePath -> FilePath -> FilePath -> FilePath -> IO [SPointer]
processSynsets trashold pathSyns pathSugg pathVote pathFreq =
  (map fst . sortBy f) <$> filtered
  where
    f x y = compare (snd y) (snd x)
    filtered = frequencyFilter trashold <$> (filteredSens pathSyns pathSugg pathVote) <*> (filteredFreq pathFreq)

filteredFreq :: FilePath -> IO [Frequency]
filteredFreq pathFreq = (parseFrequencies) <$> (getFrequencies pathFreq)

filteredSens :: [Char] -> [Char] -> [Char] -> IO [SPointer]
filteredSens pathSyns pathSugg pathVote =
  (groupSensesWordB . collectRelationsSenses) <$> (c4 <$> synsDocs <*> suggFilter)
  where
    idFilter = fmap (c0 1) scoreIds
    suggFilter = c2 <$> suggDocs <*> idFilter
    synsDocs = fmap (f1) (readJL readSynset pathSyns)
    suggDocs = fmap (c1 . f1) (readJL readSuggestion pathSugg)
    scoreIds = fmap (f3 . f2 . f1) (readJL readVote pathVote)
