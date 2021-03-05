{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Solr where

import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Either
import Data.List
import GHC.Generics


data Vote =
  Vote
    { v_id :: String
    , date :: Integer
    , suggestion_id :: String
    , user :: String
    , value :: Integer
    }
  deriving (Show, Generic)

data Suggestion =
  Suggestion
    { status :: String
    , user :: Maybe String
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


-- records do not allow parameters! This is a problem
-- here. alternatives? a Document wraps Synset, Suggestion, Vote etc.

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
          "v_id" -> "id"
          "s_id" -> "id"
          label -> label
    }

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


{- UPDATE SYNSETS -}

f1 :: [Either String (Document a)] -> [a]
f1 = map _source . rights

f2 :: [Vote] -> [[Vote]]
f2 xs = groupBy fg (sortBy fo xs)
  where
    fg = \x y -> suggestion_id x == suggestion_id y
    fo = \x y -> suggestion_id x `compare` suggestion_id y

-- NOTE: lists (suggestion id, Score)
f3 :: [[Vote]] -> [(String, Integer)]
f3 =
  map (\x -> (i x, s x)) 
  where
    i = suggestion_id . head 
    s = sum . map value

-- NOTE: simple acception/rejection rule
-- NOTE: filters ids with score >= treshold
c0 :: Integer -> [(String, Integer)] -> [String]
c0 th ids_score =
  [id | (id, sc) <- ids_score, sc >= th]

-- NOTE: remove comments and commited
c1 :: [Suggestion] -> [Suggestion]
c1 ss =
  filter (\s -> (f_status s && not (f_comment s))) ss
  where
    f_status s = status s == "new"
    f_comment s = action s == "comment"

-- NOTE: filters suggestion with valid ids
c2 :: [Suggestion] -> [String] -> [Suggestion]
c2 ss ids =
  filter f_ids ss
  where
    f_ids s = elem (s_id s) ids

-- NOTE: groups suggestions for synset
-- c3 :: [Synset] -> [Suggestion] -> [(Synset,[Suggestion])]
-- c3 sns sgs =
--   [(sn, zips sn sgs) | sn <- sns]
--   where
--     zips sn sgs = filter (\sg -> grps sn sg) sgs
--     grps sn sg = (doc_id sn) == (synset_id sg)
c3 :: [Synset] -> [Suggestion] -> [(Synset,[Suggestion])]
c3 sns sgs =
  group_sns_sgs [] sorted_sns grouped_sgs
  where
    sorted_sns = sortBy compare_sns sns
    compare_sns x y = compare (doc_id x) (doc_id y)
    grouped_sgs = groupBy group_sgs (sortBy compare_sgs sgs)
    group_sgs x y = (synset_id x) == (synset_id y)
    compare_sgs x y = compare (synset_id x) (synset_id y)
    group_sns_sgs out [] _ = out
    group_sns_sgs out ss [] = [(s,[]) | s <- ss] ++ out
    group_sns_sgs out (s:ss) (sg:sgs) =
      case (compare (doc_id s) (synset_id (head sg))) of
        GT -> group_sns_sgs out (s:ss) sgs
        EQ -> group_sns_sgs ((s,sg):out) ss sgs
        LT -> group_sns_sgs ((s,[]):out) ss (sg:sgs)

c4 :: [Synset] -> [Suggestion] -> [Synset]
c4 synsets suggestions =
  [updateSynset ta tb | (ta, tb) <- re]
  where
    re = c3 synsets suggestions

-- NOTE: compose
f =
  c4 <$> sy_doc <*> sg_filter
  where
    id_filter = fmap (c0 1) id_scores
    sg_filter = c2 <$> sg_doc <*> id_filter
    sy_doc = fmap (f1) (readJL readSynset "/home/fredson/openWordnet-PT/dump/wn.json")
    sg_doc = fmap (c1 . f1) (readJL readSuggestion "/home/fredson/openWordnet-PT/dump/suggestion.json.short")
    id_scores = fmap (f3 . f2 . f1) (readJL readVote "/home/fredson/openWordnet-PT/dump/votes.json")

-- NOTE: main rules funcction
-- NOTE: found actions using: cat path/to/suggestion.json | grep -oP "(?<=(\"action\":))\"[a-z-]+\"" | sort | uniq

-- NOTE: discuss optimal solution
applySuggestion :: Synset -> Suggestion -> Synset
applySuggestion sn sg =
  case (action sg) of
    "remove-example-pt" -> sn {example_pt = removeItem (params sg) (example_pt sn)}
    "remove-gloss-pt" -> sn {gloss_pt = removeItem (params sg) (gloss_pt sn)}
    "remove-word-pt" -> sn {word_pt = removeItem (params sg) (word_pt sn)}
    "add-example-pt" -> sn {example_pt = addItem (params sg) (example_pt sn)}
    "add-gloss-pt" -> sn {gloss_pt = addItem (params sg) (example_pt sn)}
    "add-word-pt" -> sn {word_pt = addItem (params sg) (example_pt sn)}
    --sn _ -> sn
  where
    addItem x Nothing = Just [x]
    addItem x (Just ys) = Just (x:ys)
    removeItem x Nothing = Nothing
    removeItem x (Just ys) = Just (filter (==x) ys)

updateSynset :: Synset -> [Suggestion] -> Synset
updateSynset sn [] = sn
updateSynset sn (sg:sgs) =
  updateSynset (applySuggestion sn sg) sgs

-- do we have any error?
-- fmap (nub . lefts) (readJL readVote "/Users/ar/work/wn/openWordnet-PT/tmp/dump/votes.json")

-- TODO
-- c0 :: Integer -> [(String, Integer)] -> [(Suggestions, Integer)]
-- c1 :: [Suggestion, Inteter] -> [Suggestion->Bool] -> [Suggestion]
