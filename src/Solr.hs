{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Solr where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
-- import qualified Data.ByteString.Lazy.Char8 as C
import GHC.Generics
import Data.Either
import Data.List


data Pointer = Pointer
  { target_synset :: String
  , pointer :: String
  , target_word :: Maybe String
  , source_word :: Maybe String
  , name :: Maybe String
  } deriving (Show, Generic)

data Synset = Synset {
  word_count_pt :: Int,
  word_count_en :: Int,  
  wn30_synsetId :: [String],
  rdf_type :: [String],
  gloss_en :: [String],
  word_en :: [String],
  word_pt :: Maybe [String],
  wn30_lexicographerFile :: [String],
  wn30_en_classifiedByRegion :: Maybe [Pointer],
  wn30_en_hypernymOf :: Maybe [Pointer],
  wn30_en_hyponymOf :: Maybe [Pointer],
  wn30_en_antonymOf :: Maybe [Pointer],
  wn30_en_partMeronymOf :: Maybe [Pointer],
  wn30_en_memberHolonymOf :: Maybe [Pointer],
  doc_id :: String,
  wn30_en_instanceOf :: Maybe [Pointer]
} deriving (Show, Generic)  

data Document = Document {
  _index :: String,
  _type :: String,
  _id :: String,
  _score :: Int,
  _source :: Synset
} deriving (Show, Generic)

instance FromJSON Pointer
instance FromJSON Synset
instance FromJSON Document


readJ :: L.ByteString -> Either String Document
readJ s = do
  doc <- eitherDecode s :: Either String Document
  return doc

readJL :: FilePath -> IO [Either String Document]
readJL path = do
  content <- L.readFile path
  return (map readJ $ L.split 10 content)
