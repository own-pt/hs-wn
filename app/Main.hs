module Main where

import Solr
import Query
import Filter
import Update

import Data.List

import Control.Monad

import System.Exit
import System.Directory
import System.Environment

main :: IO ()
main = getArgs >>= parse

{- deals with CLI -}
parse ["-h"] = usage >> exitSuccess
parse input  = run input >> exitSuccess

usage = putStrLn "usage: [-h] pathSyns pathSugg pathVote pathFreq trashold pathOut\
                 \\n\tpathSyns - path to wn.json\
                 \\n\tpathSugg - path to suggestions.json\
                 \\n\tpathVote - path to votes.json\
                 \\n\tpathFreq - path to frequencies\
                 \\n\ttrashold - minimun acceptable fequency\
                 \\n\tpathOut - path to output folder"
        
run [] = usage
run [pathSyns,pathSugg,pathVote,pathFreq,trashold,pathOut] =
  (groupBy f . sortBy g) <$> synsets >>= (mapM_ $ putToFile pathOut)
  where
    f x y = (relation x, typeA x,typeB x) == (relation y, typeA y,typeB y)
    g x y = compare (relation x, typeA x,typeB x) (relation y, typeA y,typeB y)
    synsets = processSynsets (read trashold :: Integer) pathSyns pathSugg pathVote pathFreq
run inputs = usage


putToFile :: FilePath -> [SPointer] -> IO ()
putToFile pathOut spointers = do
  createDirectoryIfMissing True pathOut
  writeFile filename output 
  where
    first = head spointers
    output = (intercalate "\n" $ map showOut spointers)
    filename = pathOut++"/"++(relation first)++"-"++(typeA first)++"-"++(typeB first)++".txt"
    showOut spointer = (wordA spointer) ++ "\t" ++ (intercalate "/" $ wordB spointer)

{- runs the update task -}
processSynsets :: Integer -> FilePath -> FilePath -> FilePath -> FilePath -> IO [SPointer]
processSynsets trashold pathSyns pathSugg pathVote pathFreq =
  (filter g . map fst . sortBy f) <$> filtered
  where
    g x = (length $ wordB x) > 0
    f x y = compare (snd y) (snd x)
    filtered = frequencyFilter trashold <$> (filteredSyns pathSyns pathSugg pathVote) <*> (filteredFreq pathFreq)

filteredFreq pathFreq = (parseFrequencies) <$> (getFrequencies pathFreq)
filteredSyns pathSyns pathSugg pathVote =
  collectRelationsSenses <$> (c4 <$> synsDocs <*> suggFilter)
  where
    idFilter = fmap (c0 1) scoreIds
    suggFilter = c2 <$> suggDocs <*> idFilter
    synsDocs = fmap (f1) (readJL readSynset pathSyns)
    suggDocs = fmap (c1 . f1) (readJL readSuggestion pathSugg)
    scoreIds = fmap (f3 . f2 . f1) (readJL readVote pathVote)
