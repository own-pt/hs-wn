module Main where

import Lib
import Solr

main :: IO ()
main = do
  a <- show <$> f
  putStrLn a
