{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Solr where

import Data.List
import Data.Aeson
import Data.Maybe
import Data.Either
import GHC.Generics
import qualified Data.ByteString.Lazy as L


type Sense = String
type RDFType = String
type Relation = String

data Vote =
  Vote
    { v_id :: String
    , date :: Integer
    , suggestion_id :: String
    , v_user :: String
    , value :: Integer
    }
  deriving (Show, Generic)

data Suggestion =
  Suggestion
    { status :: String
    , s_user :: Maybe String
    , params :: String
    , s_id :: String
    , action :: String
    , stype :: String
    , tags :: Maybe [String]
    , sum_votes :: Maybe Int
    , vote_score :: Maybe Int    
    , all_voters :: Maybe [String]
    , negative_votes :: Maybe [Int]
    , negative_voters :: Maybe [String]
    , positive_voters :: Maybe [String]
    , positive_votes :: Maybe [Int]
    , provenance :: String
    , date :: Integer
    , synset_id :: String
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
    , gloss_pt :: Maybe [String]
    , word_en :: [String]
    , word_pt :: Maybe [String]
    , example_pt :: Maybe [String]
    , doc_id :: String
    , wn30_frame :: Maybe [String]
    , wn30_lexicographerFile :: [String]
    , wn30_similarTo  :: Maybe [String]
    -- relations
    , wn30_en_adjectivePertainsTo :: Maybe [Pointer]
    , wn30_en_adverbPertainsTo :: Maybe [Pointer]
    , wn30_en_antonymOf :: Maybe [Pointer]
    , wn30_pt_antonymOf :: Maybe [Pointer]
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
    , wn30_pt_property :: Maybe [Pointer]
    , wn30_en_result :: Maybe [Pointer]
    , wn30_pt_result :: Maybe [Pointer]
    , wn30_en_state :: Maybe [Pointer]
    , wn30_pt_state :: Maybe [Pointer]
    , wn30_en_undergoer :: Maybe [Pointer]
    , wn30_pt_undergoer :: Maybe [Pointer]
    , wn30_en_uses :: Maybe [Pointer]
    , wn30_pt_uses :: Maybe [Pointer]
    , wn30_en_vehicle :: Maybe [Pointer]
    , wn30_pt_vehicle :: Maybe [Pointer]
    , wn30_en_entails :: Maybe [Pointer]
    , wn30_en_event :: Maybe [Pointer]
    , wn30_pt_event :: Maybe [Pointer]
    , wn30_en_instrument :: Maybe [Pointer]
    , wn30_pt_instrument :: Maybe [Pointer]
    , wn30_en_location :: Maybe [Pointer]
    , wn30_pt_location :: Maybe [Pointer]
    , wn30_en_material :: Maybe [Pointer]
    , wn30_pt_material :: Maybe [Pointer]
    , wn30_en_agent :: Maybe [Pointer]
    , wn30_pt_agent :: Maybe [Pointer]
    , wn30_en_bodyPart :: Maybe [Pointer]
    , wn30_pt_bodyPart :: Maybe [Pointer]
    , wn30_en_byMeansOf :: Maybe [Pointer]
    , wn30_pt_byMeansOf :: Maybe [Pointer]
    , wn30_en_destination :: Maybe [Pointer]
    }
  deriving (Show, Generic)


data Document a =
  Document
    { _index :: String
    , _type :: String
    , _id :: String
    , _score :: Int
    , _source :: a
    }
  deriving (Show, Generic)

customOps =
  defaultOptions
    { rejectUnknownFields = True
    , fieldLabelModifier =
        \label -> case label of
          "synset_id" -> "doc_id"
          "stype" -> "type"
          "v_user" -> "user"
          "s_user" -> "user"
          "v_id" -> "id"
          "s_id" -> "id"
          label -> label
    }

instance Eq Vote where
  (==) x y = (==) (v_id x) (v_id y)
instance Ord Vote where
  (<=) x y = (<=) (v_id x) (v_id y)
  
instance Eq Synset where
  (==) x y = (==) (doc_id x) (doc_id y)
instance Ord Synset where
  (<=) x y = (<=) (doc_id x) (doc_id y)

instance Eq Suggestion where
  (==) x y = (==) (s_id x) (s_id y)
instance Ord Suggestion where
  (<=) x y = (<=) (s_id x) (s_id y)


{- READING DOCUMENTS -}

instance FromJSON Synset where
  parseJSON = genericParseJSON customOps

instance FromJSON Suggestion where
  parseJSON = genericParseJSON customOps
instance FromJSON Pointer
instance FromJSON Vote where
  parseJSON = genericParseJSON customOps

instance FromJSON a => FromJSON (Document a)

readSuggestion s = eitherDecode s :: Either String (Document Suggestion)
readSynset s = eitherDecode s :: Either String (Document Synset)
readVote s = eitherDecode s :: Either String (Document Vote)

readJL :: (L.ByteString -> b) -> FilePath -> IO [b]
readJL reader path = do
  content <- L.readFile path
  return (map reader $ filter (not . L.null) (L.split 10 content))
