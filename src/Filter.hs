{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

{-
adds a frequence filter to words based on occurrences on texts of CCBB,
which describes a portuguese historical-political corpus

for details see: github.com/cpdoc/dhbb-nlp
for file examples see: github.com/cpdoc/dhbb-nlp/blob/master/udp

file with frequencies generated using on the directory:
awk '$0 ~ /^[0-9]/ {print $3,$4}' *.conllu | sort | uniq -c | sort -nr
-}

module Process where

import Solr
import Query
import Text.ParserCombinators.ReadP

data Frequency = Frequency
  { freq :: Integer
  , word :: String
  , pos :: String
  } deriving (Show)

getFrequencies :: FilePath -> IO String
getFrequencies path = do
  file <- readFile path
  return file

-- supose files like: FREQ WORD POS
parseFrequency :: [String] -> Frequency
parseFrequency (f:w:pos:[]) = Frequency (read f :: Integer) w pos


parseFrequencies :: String -> [Frequency]
parseFrequencies input =
  (map parseFrequency . map words . lines) $ input


{- Filtering -}
