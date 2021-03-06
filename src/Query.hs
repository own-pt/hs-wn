{-# LANGUAGE DeriveGeneric, OverloadedStrings, DuplicateRecordFields #-}

module Query where

import Solr
import Data.Maybe
import Data.List

{-
The attemp is to group elements of form (?a,?ta,?b,?b,?rel) given synsets
from portuguese related to synsets in english that sustain a relation ?rel

 - ?a and ?b are word/lexicalform
 - ?ta and ?tb are synset types
 - ?rel is a relation

see: github.com/NLP-CISUC/PT-LexicalSemantics/blob/master/OWN-PT/query.sparql
-}

query :: [Synset] -> [(String,String,String,String,String)]
query synsets =
  [(a,ta,b,tb,rel) | (syna,rel,synb) <- collectRelationSynsets synsets
                   , a <- fromMaybe [] (word_pt syna)
                   , ta <- (rdf_type syna)
                   , b <- fromMaybe [] (word_pt synb)
                   , tb <- (rdf_type synb)]
  

collectRelationSynsets :: [Synset] -> [(Synset, String, Synset)]
collectRelationSynsets synsets =
  collectSynsets [] (sortBy cRelIds relationIds) (sort synsets)
  where
    trd (_,_,x) = x
    cRelIds x y = compare (trd x) (trd y)
    relationIds = collectRelationIds synsets
    collectSynsets out [] _ = out
    -- collectSynsets out _ [] = out
    collectSynsets out ((syn,rel,id):rids) (sy:sys) =
      case (compare id (doc_id sy)) of
        GT -> collectSynsets out ((syn,rel,id):rids) sys
        EQ -> collectSynsets ((syn,rel,sy):out) rids (sy:sys)
        -- LT -> collectSynsets out rids (sy:sys)
    
collectRelationIds :: [Synset] -> [(Synset, String, String)]
collectRelationIds synsets =
  [(sy, pointer pt, target_synset pt) | sy <- synsets
                                      , pt <- collectPointers sy]

collectPointers :: Synset -> [Pointer]
collectPointers synset =
  concat . (map fromJust) . (filter isJust) $ pointers
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
