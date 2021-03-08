{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Query where

import Solr
import Update
import Data.List
import Data.Char
import Data.Maybe

{-
The attemp is to group elements of form (?a,?b,?ta,?tb,?rel) given synsets
from portuguese related to synsets in english that sustain a relation ?rel

 - ?a and ?b are word/lexicalform
 - ?ta and ?tb are synset types
 - ?rel is a relation

see: github.com/NLP-CISUC/PT-LexicalSemantics/blob/master/OWN-PT/query.sparql
-}

data SPointer = SPointer
  { wordA :: Sense
  , wordB :: Sense
  , typeA :: RDFType
  , typeB :: RDFType
  -- , docIdA :: String
  -- , docIdB :: String
  , relation :: Relation
  } deriving (Show)

instance Eq SPointer where
  (==) x y = (==) (sPointerToTuple x) (sPointerToTuple y)

instance Ord SPointer where
  (<=) x y = (<=) (sPointerToTuple x) (sPointerToTuple y)

sPointerToTuple (SPointer a b ta tb rel) = (a,b,ta,tb,rel)
tupleToSPointer (a,b,ta,tb,rel) = (SPointer a b ta tb rel)


groupSensesWordB :: [SPointer] -> [SPointer]
groupSensesWordB spointers =
  (map g3 . groupBy g2 . sortBy g1) spointers
  where
    g x = (wordA x, relation x, typeA x, typeB x)
    g1 x y = compare (g x) (g y)
    g2 x y = (==) (g x) (g y)
    first = head spointers
    source = wordA first
    g3 spointers = first {wordB = group_words spointers}
    dsource source targets = filter (/=source) targets
    dduplicates spointers =  (map head . group . sort) spointers
    group_words spointers = (intercalate "/" . dsource source . dduplicates . map wordB) spointers

    
collectSensePointers :: [Synset] -> [SPointer]
collectSensePointers synsets =
  [ SPointer (map toLowerSub a) (map toLowerSub b) ta tb (pointer p)
  | (synA,p,synB) <- collectPointersSynsets synsets
  , a <- choseSenseWords synA (source_word p)
  , b <- choseSenseWords synB (target_word p)
  , ta <- rdf_type synA
  , tb <- rdf_type synB]
   where
    toLowerSub = (subs ' ' '_') . toLower
    subs a b c = if c == a then b else c
    choseSenseWords synX Nothing = fromMaybe [] (word_pt synX)
    choseSenseWords synX word = [fromJust word]

  
collectPointersSynsets :: [Synset] -> [(Synset, Pointer, Synset)]
collectPointersSynsets synsets =
  collectSynsets [] (sortBy compareIds relationIds) (sort synsets)
  where
    compareIds x y = compare (target_synset $ snd x) (target_synset $ snd y)
    relationIds = [(s,p) | s <- synsets, p <- collectPointers s]


collectSynsets out [] _ = out
collectSynsets out (rid:rids) (syn:syns) =
  case (compare (target_synset $ snd rid) (doc_id syn)) of
    GT -> collectSynsets out (rid:rids) syns
    EQ -> collectSynsets ((fst rid, snd rid, syn):out) rids (syn:syns)


collectPointers :: Synset -> [Pointer]
collectPointers synset =
  concat $ map (fromMaybe []) $ pointers
  where
    pointers = [relation synset | relation <- relations]


-- relation fuctions
relations :: [Synset -> Maybe [Pointer]]
relations = [
  wn30_en_adjectivePertainsTo,
  wn30_en_adverbPertainsTo,
  wn30_en_antonymOf,
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
  wn30_en_substanceHolonymOf]
