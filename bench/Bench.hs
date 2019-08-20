{-# LANGUAGE RankNTypes #-}

module Main where

import Gauge.Main
import qualified DenseBench as Dense
import qualified LaurentBench as Laurent
import qualified SparseBench as Sparse

main :: IO ()
main = defaultMain
  [ Dense.benchSuite
  , Laurent.benchSuite
  , Sparse.benchSuite
  ]
