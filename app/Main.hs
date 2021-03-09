module Main where

import Lib ( processSynsets )
import Query ( SPointer(wordA, wordB, relation, typeA, typeB) )

import Data.List ( intercalate, sortBy, groupBy )

import System.Exit ( exitSuccess )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )


main :: IO ()
main = getArgs >>= parse


{- Command Line Interface -}
parse :: [[Char]] -> IO b
parse ["-h"] = usage >> exitSuccess
parse input  = apply input >> exitSuccess

usage :: IO ()
usage = putStrLn "Usage: [-h] pathSyns pathSugg pathVote pathFreq trashold pathOut\
                 \\n\tpathSyns - path to wn.json\
                 \\n\tpathSugg - path to suggestions.json\
                 \\n\tpathVote - path to votes.json\
                 \\n\tpathFreq - path to frequencies\
                 \\n\ttrashold - minimun acceptable fequency\
                 \\n\tpathOut - path to output folder"


apply :: [FilePath] -> IO ()
apply [] = usage
apply [pathSyns,pathSugg,pathVote,pathFreq,trashold,pathOut] =
  synsets >>= mapM_ (save pathOut) . groupBy f . sortBy g
  where
    f x y = (relation x, typeA x,typeB x) == (relation y, typeA y,typeB y)
    g x y = compare (relation x, typeA x,typeB x) (relation y, typeA y,typeB y)
    synsets = processSynsets (read trashold :: Integer) pathSyns pathSugg pathVote pathFreq
apply inputs = usage


save :: FilePath -> [SPointer] -> IO ()
save pathOut spointers = do
  createDirectoryIfMissing True pathOut
  writeFile filepath output 
  where
    first = head spointers
    output = intercalate "\n" (map showOut spointers)
    showOut spointer = wordA spointer ++ "\t" ++ wordB spointer
    filename = relation first ++ "-" ++ typeA first ++ "-"++ typeB first ++ ".txt"
    filepath = pathOut ++ "/" ++ filename

