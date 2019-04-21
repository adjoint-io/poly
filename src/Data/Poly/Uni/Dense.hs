-- |
-- Module:      Data.Poly.Uni.Dense
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials of one variable.
--

{-# LANGUAGE ScopedTypeVariables #-}

module Data.Poly.Uni.Dense
  ( Poly
  , unPoly
  , toPoly
  , toPoly'
  , eval
  , eval'
  , deriv
  , deriv'
  , var
  , var'
  ) where

import Control.Monad
import Control.Monad.ST
import Data.List (foldl')
import Data.Semigroup (stimes)
import Data.Semiring (Semiring(..), Add(..))
import qualified Data.Semiring as Semiring
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV

-- | Polynomials of one variable.
--
-- >>> :set -XOverloadedLists
-- >>> -- (1 + x) * (-1 + x) = (-1 + x^2)
-- >>> toPoly [1,1] * toPoly [-1,1]
-- Poly {unPoly = [-1,0,1]}
--
-- >>> :set -XOverloadedLists
-- >>> -- (1 + x) + (1 - x) = 2
-- >>> toPoly [1,1] + toPoly [1,-1]
-- Poly {unPoly = [2]}
newtype Poly a = Poly
  { unPoly :: Vector a
  -- ^ Convert 'Poly' to a vector of coefficients
  -- (first element corresponds to a constant term).
  }
  deriving (Eq, Ord, Show)

-- | Make 'Poly' from a list of coefficients
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [1,2,3]
-- Poly {unPoly = [1,2,3]}
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [0,0,0]
-- Poly {unPoly = []}
toPoly :: (Eq a, Num a) => Vector a -> Poly a
toPoly = Poly . dropWhileEnd (== 0)

-- | Make 'Poly' from a vector of coefficients
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly' [1,2,3]
-- Poly {unPoly = [1,2,3]}
--
-- >>> :set -XOverloadedLists
-- >>> toPoly' [0,0,0]
-- Poly {unPoly = []}
toPoly' :: (Eq a, Semiring a) => Vector a -> Poly a
toPoly' = Poly . dropWhileEnd (== zero)

instance (Eq a, Num a) => Num (Poly a) where
  Poly xs + Poly ys = toPoly $ plusPoly (+) xs ys
  Poly xs - Poly ys = toPoly $ minusPoly negate (-) xs ys
  negate (Poly xs) = Poly $ V.map negate xs
  abs = id
  signum = const 1
  fromInteger n = case fromInteger n of
    0 -> Poly $ V.empty
    m -> Poly $ V.singleton m
  Poly xs * Poly ys = toPoly $ convolution 0 (+) (*) xs ys

instance (Eq a, Semiring a) => Semiring (Poly a) where
  zero = Poly V.empty
  one
    | (one :: a) == zero = zero
    | otherwise = Poly $ V.singleton one
  plus (Poly xs) (Poly ys) = toPoly' $ plusPoly plus xs ys
  times (Poly xs) (Poly ys) = toPoly' $ convolution zero plus times xs ys

instance (Eq a, Semiring.Ring a) => Semiring.Ring (Poly a) where
  negate (Poly xs) = Poly $ V.map Semiring.negate xs

dropWhileEnd :: (a -> Bool) -> Vector a -> Vector a
dropWhileEnd p xs = V.slice 0 (go (V.length xs)) xs
  where
    go 0 = 0
    go n = if p (xs V.! (n - 1)) then go (n - 1) else n

plusPoly :: (a -> a -> a) -> Vector a -> Vector a -> Vector a
plusPoly add xs ys = runST $ do
  zs <- MV.new (max lenXs lenYs)
  case lenXs `compare` lenYs of
    LT -> do
      forM_ [0 .. lenXs - 1] $ \i ->
        MV.write zs i (add (xs V.! i) (ys V.! i))
      V.copy (MV.slice lenXs (lenYs - lenXs) zs) (V.slice lenXs (lenYs - lenXs) ys)
    EQ -> do
      forM_ [0 .. lenXs - 1] $ \i ->
        MV.write zs i (add (xs V.! i) (ys V.! i))
    GT -> do
      forM_ [0 .. lenYs - 1] $ \i ->
        MV.write zs i (add (xs V.! i) (ys V.! i))
      V.copy (MV.slice lenYs (lenXs - lenYs) zs) (V.slice lenYs (lenXs - lenYs) xs)
  V.unsafeFreeze zs
  where
    lenXs = V.length xs
    lenYs = V.length ys

minusPoly :: (a -> a) -> (a -> a -> a) -> Vector a -> Vector a -> Vector a
minusPoly neg sub xs ys = runST $ do
  zs <- MV.new (max lenXs lenYs)
  case lenXs `compare` lenYs of
    LT -> do
      forM_ [0 .. lenXs - 1] $ \i ->
        MV.write zs i (sub (xs V.! i) (ys V.! i))
      forM_ [lenXs .. lenYs - 1] $ \i ->
        MV.write zs i (neg (ys V.! i))
    EQ -> do
      forM_ [0 .. lenXs - 1] $ \i ->
        MV.write zs i (sub (xs V.! i) (ys V.! i))
    GT -> do
      forM_ [0 .. lenYs - 1] $ \i ->
        MV.write zs i (sub (xs V.! i) (ys V.! i))
      V.copy (MV.slice lenYs (lenXs - lenYs) zs) (V.slice lenYs (lenXs - lenYs) xs)
  V.unsafeFreeze zs
  where
    lenXs = V.length xs
    lenYs = V.length ys

convolution :: a -> (a -> a -> a) -> (a -> a -> a) -> Vector a -> Vector a -> Vector a
convolution zer add mul xs ys
  | V.null xs || V.null ys = V.empty
  | otherwise = runST $ do
    zs <- MV.new lenZs
    forM_ [0 .. lenZs - 1] $ \k -> do
      let is = [max (k - lenYs + 1) 0 .. min k (lenXs - 1)]
          -- js = reverse [max (k - lenXs) 0 .. min k lenYs]
      let acc = foldl' add zer $ flip map is $ \i -> mul (xs V.! i) (ys V.! (k - i))
      MV.write zs k acc
    V.unsafeFreeze zs
  where
    lenXs = V.length xs
    lenYs = V.length ys
    lenZs = lenXs + lenYs - 1

eval :: Num a => Poly a -> a -> a
eval (Poly cs) x = fst $
  V.foldl' (\(acc, xn) cn -> (acc + cn * xn, x * xn)) (0, 1) cs

eval' :: Semiring a => Poly a -> a -> a
eval' (Poly cs) x = fst $
  V.foldl' (\(acc, xn) cn -> (acc `plus` cn `times` xn, x `times` xn)) (zero, one) cs

deriv :: (Eq a, Num a) => Poly a -> Poly a
deriv (Poly xs)
  | V.null xs = Poly V.empty
  | otherwise = toPoly $ V.imap (\i x -> fromIntegral (i + 1) * x) $ V.tail xs

deriv' :: (Eq a, Semiring a) => Poly a -> Poly a
deriv' (Poly xs)
  | V.null xs = Poly V.empty
  | otherwise = toPoly' $ V.imap (\i x -> getAdd (stimes (i + 1) (Add x))) $ V.tail xs

var :: forall a. (Eq a, Num a) => Poly a
var
  | (1 :: a) == 0 = Poly V.empty
  | otherwise     = Poly $ V.fromList [0, 1]

var' :: forall a. (Eq a, Semiring a) => Poly a
var'
  | (one :: a) == zero = Poly V.empty
  | otherwise          = Poly $ V.fromList [zero, one]
