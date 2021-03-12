{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

{-
The attemp is to group elements of form (?a,?ta,?b,?tb,?rel) given synsets
from portuguese related to synsets in english that sustain a relation ?rel

 - ?a and ?b are word/lexicalform
 - ?ta and ?tb are synset types
 - ?rel is a relation

see: github.com/NLP-CISUC/PT-LexicalSemantics/blob/master/OWN-PT/query.sparql
-}

module Query where

import Data.List ( intercalate, sortBy, groupBy, group, sort )
import Data.Char ( toLower )
import Data.Maybe ( fromJust, fromMaybe )
import qualified Data.Map as M

import ReadDocs
import Polysemy

type RDFType = String
type Relation = String

data Sense = 
  Sense
  { sense :: String
  , isCore :: Bool
  , polysemy :: Integer
  , frequency :: Integer
  } deriving (Show, Eq)

instance Ord Sense where
  (<=) x y = (<=)
    (not $ isCore x, polysemy x, sense x, - frequency x)
    (not $ isCore y, polysemy y, sense y, - frequency x)

data SPointer = SPointer
  { senseA :: Sense
  , senseB :: Sense
  , typeA :: RDFType
  , typeB :: RDFType
  , relation :: Relation
  } deriving (Show, Eq, Ord)


senseFromSynset :: [Char] -> Synset -> Integer -> Integer -> Sense
senseFromSynset w s
  = Sense
      (map toLower w)
      ("CoreConcept" `elem` rdf_type s)

{- main code -}

groupSensesWordB :: [SPointer] -> [[SPointer]]
groupSensesWordB = groupBy g1 . sortBy g2 . sort
  where
    g x = (senseA x, relation x, typeA x, typeB x)
    g1 x y = (==) (g x) (g y)
    g2 x y = compare (g x) (g y)

    
collectRelationsSenses :: [Synset] -> [SPointer]
collectRelationsSenses synsets =
  [ SPointer (senseFromSynset a synA (mapPolysemy M.! a) 0) (senseFromSynset b synB (mapPolysemy M.! b) 0) ta tb (pointer p)
  | (synA,p,synB) <- collectPointersSynsets synsets
  , a <- choseSenseWords synA (source_word p)
  , b <- choseSenseWords synB (target_word p)
  , ta <- rdf_type synA
  , tb <- rdf_type synB]
   where
    subs a b c = if c == a then b else c
    choseSenseWords synX Nothing = fromMaybe [] (word_pt synX)
    choseSenseWords synX (Just word) =
      [word | word `elem` choseSenseWords synX Nothing]
    mapPolysemy = getPolysemias synsets

  
collectPointersSynsets :: [Synset] -> [(Synset, Pointer, Synset)]
collectPointersSynsets synsets =
  collectSynsets [] (sortBy compareIds relationIds) (sort synsets)
  where
    compareIds x y = compare (target_synset $ snd x) (target_synset $ snd y)
    relationIds = [(s,p) | s <- synsets, p <- collectPointers s]


collectSynsets :: [(a, Pointer, Synset)] -> [(a, Pointer)] -> [Synset] -> [(a, Pointer, Synset)]
collectSynsets out [] _ = out
collectSynsets out (rid:rids) (syn:syns) =
  case compare (target_synset $ snd rid) (doc_id syn) of
    GT -> collectSynsets out (rid:rids) syns
    EQ -> collectSynsets ((fst rid, snd rid, syn):out) rids (syn:syns)


collectPointers :: Synset -> [Pointer]
collectPointers synset =
  concatMap (fromMaybe []) pointers
  where
    pointers = [relation synset | relation <- relations]


relations :: [Synset -> Maybe [Pointer]]
relations = [
  -- relations
  wn30_en_adjectivePertainsTo,
  wn30_en_adverbPertainsTo,
  wn30_en_antonymOf,
  wn30_pt_antonymOf,
  wn30_en_attribute,
  wn30_en_causes,
  wn30_en_classifiedByRegion,
  wn30_en_classifiedByTopic,
  wn30_en_classifiedByUsage,
  wn30_en_classifiesByRegion,
  wn30_en_classifiesByTopic,
  wn30_en_classifiesByUsage,
  wn30_en_derivationallyRelated,
  wn30_en_hasInstance,
  wn30_en_hypernymOf,
  wn30_en_hyponymOf,
  wn30_en_instanceOf,
  wn30_en_memberHolonymOf,
  wn30_en_memberMeronymOf,
  wn30_en_partHolonymOf,
  wn30_en_partMeronymOf,
  wn30_en_participleOf,
  wn30_en_sameVerbGroupAs,
  wn30_en_seeAlso,
  wn30_en_substanceHolonymOf,
  wn30_en_substanceMeronymOf,
  -- morphosemantic links
  wn30_en_property,
  wn30_pt_property,
  wn30_en_result,
  wn30_pt_result,
  wn30_en_state,
  wn30_pt_state,
  wn30_en_undergoer,
  wn30_pt_undergoer,
  wn30_en_uses,
  wn30_pt_uses,
  wn30_en_vehicle,
  wn30_pt_vehicle,
  wn30_en_entails,
  wn30_en_event,
  wn30_pt_event,
  wn30_en_instrument,
  wn30_pt_instrument,
  wn30_en_location,
  wn30_pt_location,
  wn30_en_material,
  wn30_pt_material,
  wn30_en_agent,
  wn30_pt_agent,
  wn30_en_bodyPart,
  wn30_pt_bodyPart,
  wn30_en_byMeansOf,
  wn30_pt_byMeansOf,
  wn30_en_destination]
