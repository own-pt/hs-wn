{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Main where

import Lib
import Query

import Data.List
import Data.Maybe

import System.Exit ( exitSuccess )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )


main :: IO ()
main = getArgs >>= parse


{- Command Line Interface -}

usage :: IO ()
usage = putStrLn "Usage: [-h] pathSyns pathSugg pathVote [pathOut] [lines]\
                 \\n\tpathSyns - path to wn.json\
                 \\n\tpathSugg - path to suggestions.json\
                 \\n\tpathVote - path to votes.json\
                 \\n\tpathOut - path to output folder (default: output)\
                 \\n\tlines - minimun lines to write the file (default: 30)"


parse :: [[Char]] -> IO b
parse input  = apply input >> exitSuccess


apply :: [FilePath] -> IO ()
-- help
apply ["-h"] = usage
-- default: output
apply [pathSyns,pathSugg,pathVote] =
  apply [pathSyns,pathSugg,pathVote,"output"]
-- default: 30
apply [pathSyns,pathSugg,pathVote,pathOut] =
  apply [pathSyns,pathSugg,pathVote,pathOut,"30"]
-- normal use
apply [pathSyns,pathSugg,pathVote,pathOut,lines] =
  spointers >>= mapM_ (save pathOut _lines) . groupBy g1 . sortBy g2
  where
    _lines = read lines :: Int
    f x = (relation x, typeA x, typeB x)
    g1 x y = (==) (f x) (f y)
    g2 x y = compare (f x) (f y)
    spointers = updatedSPointers pathSyns pathSugg pathVote
-- default use
apply inputs = usage


-- NOTE: serializes output
save :: FilePath -> Int -> [SPointer] -> IO ()
save pathOut lines spointers = do
  createDirectoryIfMissing True pathOut
  if isJust output
    then writeFile filepath (fromJust output)
    else putStrLn ("file " ++ filename ++ " not generated.")
  where
    output = formatOutput lines spointers
    first = head spointers
    filepath = pathOut ++ "/" ++ filename
    filename = relation first ++ "-" ++ typeA first ++ "-"++ typeB first ++ ".txt"


{- formats output -}

-- NOTE: formats all output
formatOutput :: Int -> [SPointer] -> Maybe String
formatOutput lines spointers =
  if length output >= lines 
    then Just (concat output)
    else Nothing
  where
    output = mapMaybe _formatOutput grouped
    g1 x y = (==) (sense $ senseA x) (sense $ senseA y)
    g2 x y = compare (sense $ senseA x) (sense $ senseA y)
    grouped = groupBy g1 $ (sortBy g2 spointers)

-- NOTE: formats an output line
_formatOutput :: [SPointer] -> Maybe String
_formatOutput spointers =
  if rule1 sensea && rule2 sensea && rule3 sensea && rule3 sensesb
    then Just (sensea ++ "\t" ++ sensesb ++ "\n")
    else Nothing
  where
    sensea = (sense . senseA . head) spointers
    sensesb = intercalate "/" filteredsensesb
    -- group sensesB
    g x y = (==) (sense x) (sense y)
    sortedsensesb = map (sense . head) $ groupBy g $ sort $ map senseB spointers
    filteredsensesb = [s | s <- sortedsensesb, rule1 s, rule2 s, rule3 s, rule4 s]
    -- sense rules
    rule1 sense = notElem ' ' sense
    rule2 sense = notElem '_' sense
    rule3 sense = length sense > 0
    rule4 sense = sense /= sensea
