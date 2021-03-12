{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

--module Main where

import Lib
import Query

import Data.List ( intercalate, sort, sortBy, group, groupBy, elem, notElem)

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
                 \\n\tpathOutput - path to output folder"


apply :: [FilePath] -> IO ()
apply [] = usage
apply [pathSyns,pathSugg,pathVote,pathOut] =
  spointers >>= mapM_ (save pathOut) . groupBy g1 . sortBy g2
  where
    f x = (relation x, typeA x, typeB x)
    g1 x y = (==) (f x) (f y)
    g2 x y = compare (f x) (f y)
    spointers = updatedSPointers pathSyns pathSugg pathVote
apply inputs = usage


save :: FilePath -> [SPointer] -> IO ()
save pathOut spointers = do
  createDirectoryIfMissing True pathOut
  if length output > 30
    then
      writeFile filepath (concat output++"\n")
    else
      putStrLn ("file " ++ filename ++ " not generated.")
  where
    output = formatOutput spointers
    first = head spointers
    filepath = pathOut ++ "/" ++ filename
    filename = relation first ++ "-" ++ typeA first ++ "-"++ typeB first ++ ".txt"


{- formating rules for a group -}

formatOutput :: [SPointer] -> [String]
formatOutput spointers =
  map _formatOutput grouped
  where
    g1 x y = (==) (sense $ senseA x) (sense $ senseA y)
    g2 x y = compare (sense $ senseA x) (sense $ senseA y)
    grouped = groupBy g1 $ (sortBy g2 spointers)


_formatOutput :: [SPointer] -> String
_formatOutput spointers =
  if rule1 sensea && rule2 sensea && rule3 sensea && rule3 sensesb
    then sensea ++ "\t" ++ sensesb ++ "\n"
    else ""
  where
    sensea = (sense . senseA . head) spointers
    sensesb = intercalate "/" filteredsensesb
    sortedsensesb = map sense $ sort (map senseB spointers)
    filteredsensesb = [s | s <- sortedsensesb, rule1 s, rule2 s, rule3 s, rule4 s]
    -- rules
    rule1 sense = notElem ' ' sense
    rule2 sense = notElem '_' sense
    rule3 sense = length sense > 0
    rule4 sense = sense /= sensea