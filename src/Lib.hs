module Lib where

import Solr
import Query
import Update
import Process

libFunc :: IO ()
libFunc = do
  out <- (show . collectRelationsSenses) <$> f
  putStrLn out
