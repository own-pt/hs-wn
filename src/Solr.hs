{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Solr where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Either
import Data.List
import GHC.Generics


data Vote =
  Vote
    { id :: String
    , date :: Integer
    , suggestion_id :: String
    , user :: String
    , value :: Integer
    }
  deriving (Show, Generic)

data Suggestion =
  Suggestion
    { status :: String
    , user :: String
    , params :: String
    , id :: String
    , action :: String
    , stype :: String
    , vote_score :: Int
    , sum_votes :: Int
    , provenance :: String
    , date :: Integer
    , doc_id :: String
    , doc_type :: String
    }
  deriving (Show, Generic)

data Pointer =
  Pointer
    { target_synset :: String
    , pointer :: String
    , target_word :: Maybe String
    , source_word :: Maybe String
    , name :: Maybe String
    }
  deriving (Show, Generic)

-- relations and morphosemantic links were extracted from the wn.json with
-- for s in `egrep -o  "wn30_en_[a-zA-Z]+" wn.json | sort | uniq  | sort`; do echo $s ":: Maybe [Pointer]," ; done
data Synset =
  Synset
    { word_count_pt :: Int
    , word_count_en :: Int
    , wn30_synsetId :: [String]
    , rdf_type :: [String]
    , gloss_en :: [String]
    , word_en :: [String]
    , word_pt :: Maybe [String]
    , doc_id :: String
    , wn30_lexicographerFile :: [String]
    -- relations
    , wn30_en_adjectivePertainsTo :: Maybe [Pointer]
    , wn30_en_adverbPertainsTo :: Maybe [Pointer]
    , wn30_en_antonymOf :: Maybe [Pointer]
    , wn30_en_attribute :: Maybe [Pointer]
    , wn30_en_causes :: Maybe [Pointer]
    , wn30_en_classifiedByRegion :: Maybe [Pointer]
    , wn30_en_classifiedByTopic :: Maybe [Pointer]
    , wn30_en_classifiedByUsage :: Maybe [Pointer]
    , wn30_en_classifiesByRegion :: Maybe [Pointer]
    , wn30_en_classifiesByTopic :: Maybe [Pointer]
    , wn30_en_classifiesByUsage :: Maybe [Pointer]
    , wn30_en_derivationallyRelated :: Maybe [Pointer]
    , wn30_en_hasInstance :: Maybe [Pointer]
    , wn30_en_hypernymOf :: Maybe [Pointer]
    , wn30_en_hyponymOf :: Maybe [Pointer]
    , wn30_en_instanceOf :: Maybe [Pointer]
    , wn30_en_memberHolonymOf :: Maybe [Pointer]
    , wn30_en_memberMeronymOf :: Maybe [Pointer]
    , wn30_en_partHolonymOf :: Maybe [Pointer]
    , wn30_en_partMeronymOf :: Maybe [Pointer]
    , wn30_en_participleOf :: Maybe [Pointer]
    , wn30_en_sameVerbGroupAs :: Maybe [Pointer]
    , wn30_en_seeAlso :: Maybe [Pointer]
    , wn30_en_substanceHolonymOf :: Maybe [Pointer]
    , wn30_en_substanceMeronymOf :: Maybe [Pointer]
    -- morphosemantic links
    , wn30_en_property :: Maybe [Pointer]
    , wn30_en_result :: Maybe [Pointer]
    , wn30_en_state :: Maybe [Pointer]
    , wn30_en_undergoer :: Maybe [Pointer]
    , wn30_en_uses :: Maybe [Pointer]
    , wn30_en_vehicle :: Maybe [Pointer]
    , wn30_en_entails :: Maybe [Pointer]
    , wn30_en_event :: Maybe [Pointer]
    , wn30_en_instrument :: Maybe [Pointer]
    , wn30_en_location :: Maybe [Pointer]
    , wn30_en_material :: Maybe [Pointer]
    , wn30_en_agent :: Maybe [Pointer]
    , wn30_en_bodyPart :: Maybe [Pointer]
    , wn30_en_byMeansOf :: Maybe [Pointer]
    , wn30_en_destination :: Maybe [Pointer]
    }
  deriving (Show, Generic)


-- records do not allow parameters! This is a problem
-- here. alternatives? a Document wraps Synset, Suggestion, Vote etc.

data Document =
  Document
    { _index :: String
    , _type :: String
    , _id :: String
    , _score :: Int
    , _source :: Vote
    }
  deriving (Show, Generic)

customOps =
  defaultOptions
    { rejectUnknownFields = True
    , fieldLabelModifier =
        \x ->
          if x == "stype"
            then "type"
            else x
    }

instance FromJSON Synset where
  parseJSON = genericParseJSON customOps

instance FromJSON Suggestion where
  parseJSON = genericParseJSON customOps

instance FromJSON Pointer
instance FromJSON Vote
instance FromJSON Document

readJ :: L.ByteString -> Either String Document
readJ s = eitherDecode s :: Either String Document

readJL :: FilePath -> IO [Either String Document]
readJL path = do
  content <- L.readFile path
  return (map readJ $ L.split 10 content)
