{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module LaurentBench
  ( benchSuite
  ) where

import Gauge.Main
import Data.Poly.Laurent
import qualified Data.Vector.Generic as V

benchSuite :: Benchmark
benchSuite = bgroup "Laurent" $ concat
  [ map benchAdd  $ zip3 tabs vecs2 vecs3
  , map benchMul  $ take 2 $ zip3 tabs vecs2 vecs3
  , map benchEval $ zip tabs vecs2
  ]

tabs :: [Int]
tabs = [10, 100, 1000]

vecs2 :: [VPoly Rational]
vecs2 = flip map tabs $
  \n -> toPoly $ V.generate n (\i -> (fromIntegral i ^ 2, fromIntegral i * 2))

vecs3 :: [VPoly Rational]
vecs3 = flip map tabs $
  \n -> toPoly $ V.generate n (\i -> (fromIntegral i ^ 3, fromIntegral i * 3))

benchAdd :: (Int, VPoly Rational, VPoly Rational) -> Benchmark
benchAdd (k, xs, ys) = bench ("add/" ++ show k) $ nf (doBinOp (+) xs) ys

benchMul :: (Int, VPoly Rational, VPoly Rational) -> Benchmark
benchMul (k, xs, ys) = bench ("mul/" ++ show k) $ nf (doBinOp (*) xs) ys

benchEval :: (Int, VPoly Rational) -> Benchmark
benchEval (k, xs) = bench ("eval/" ++ show k) $ nf doEval xs

doBinOp :: (forall a . Num a => a -> a -> a)
  -> VPoly Rational -> VPoly Rational -> Rational
doBinOp op xs ys = V.foldl' (\acc (_, x) -> acc + x) 0 zs
  where
    zs = unPoly $ xs `op` ys
{-# INLINE doBinOp #-}

doEval :: VPoly Rational -> Rational
doEval xs = eval xs (fromIntegral (V.length (unPoly xs)))
