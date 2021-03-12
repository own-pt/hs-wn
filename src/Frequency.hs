{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

{-
adds a frequence filter to words based on occurrences on texts of CCBB,
which describes a portuguese historical-political corpus

for details see: github.com/cpdoc/dhbb-nlp
for file examples see: github.com/cpdoc/dhbb-nlp/blob/master/udp

file with frequencies generated using on the directory:
awk '$0 ~ /^[0-9]/ {print $3,$4}' *.conllu | sort | uniq -c | sort -nr
-}

module Frequency where

import Query
import Data.List ( sortBy, sort )
import Data.Char ( toLower )

data Frequency =
  Frequency
    { freq :: Integer
    , word :: String
    } deriving (Show)
    

getFrequencies :: FilePath -> IO String
getFrequencies = readFile


parseFrequency :: [String] -> Frequency
parseFrequency [] = Frequency 0 ""
parseFrequency [f] = Frequency 0 ""
parseFrequency (f:w:_) = Frequency (read f :: Integer) (prettyfy w)
  where prettyfy = map (toLower . \x -> if x == '=' then '_' else x)


parseFrequencies :: String -> [Frequency]
parseFrequencies = map (parseFrequency . words) . lines


frequencyFilter :: Integer -> [SPointer] -> [Frequency] -> [(SPointer, Integer)]
frequencyFilter trashold spointers frequencies =
  filterPointers [] trashold (sort spointers) (sortBy f frequencies)
  where
    f x y = compare (word x) (word y)
    filterPointers out th [] frs = out
    filterPointers out th sps [] = out
    filterPointers out th (sp:sps) (fr:frs) =
      case compare (sense $ senseA sp) (word fr) of
        LT -> filterPointers out th sps (fr:frs)
        GT -> filterPointers out th (sp:sps) frs
        EQ -> if freq fr >= th
              then filterPointers ((sp, freq fr):out) th sps (fr:frs)
              else filterPointers out th sps (fr:frs)
