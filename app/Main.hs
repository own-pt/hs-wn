module Main where

import Solr
import Query
import Filter
import Update
import Execute

import Data.List

import Control.Monad

import System.Exit
import System.Directory
import System.Environment

main :: IO ()
main = getArgs >>= parse


{- Command Line Interface -}
parse ["-h"] = usage >> exitSuccess
parse input  = apply input >> exitSuccess

usage = putStrLn "usage: [-h] pathSyns pathSugg pathVote pathFreq trashold pathOut\
                 \\n\tpathSyns - path to wn.json\
                 \\n\tpathSugg - path to suggestions.json\
                 \\n\tpathVote - path to votes.json\
                 \\n\tpathFreq - path to frequencies\
                 \\n\ttrashold - minimun acceptable fequency\
                 \\n\tpathOut - path to output folder"


apply [] = usage
apply [pathSyns,pathSugg,pathVote,pathFreq,trashold,pathOut] =
  (groupBy f . sortBy g) <$> synsets >>= (mapM_ $ save pathOut)
  where
    f x y = (relation x, typeA x,typeB x) == (relation y, typeA y,typeB y)
    g x y = compare (relation x, typeA x,typeB x) (relation y, typeA y,typeB y)
    synsets = processSynsets (read trashold :: Integer) pathSyns pathSugg pathVote pathFreq
apply inputs = usage


save :: FilePath -> [SPointer] -> IO ()
save pathOut spointers = do
  createDirectoryIfMissing True pathOut
  writeFile filename output 
  where
    first = head spointers
    output = (intercalate "\n" $ map showOut spointers)
    showOut spointer = (wordA spointer) ++ "\t" ++ (wordB spointer)
    filename = pathOut++"/"++(relation first)++"-"++(typeA first)++"-"++(typeB first)++".txt"

