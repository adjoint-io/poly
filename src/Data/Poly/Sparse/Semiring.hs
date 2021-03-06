-- |
-- Module:      Data.Poly.Sparse.Semiring
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse polynomials with 'Semiring' instance.
--

{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

module Data.Poly.Sparse.Semiring
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  , leading
  -- * Semiring interface
  , toPoly
  , monomial
  , scale
  , pattern X
  , eval
  , deriv
#if MIN_VERSION_semirings(0,5,0)
  , integral
#endif
#if MIN_VERSION_semirings(0,4,2)
  -- * Polynomials over 'Field'
  , gcdExt
#endif
  ) where

import Data.Semiring (Semiring)
import qualified Data.Vector.Generic as G

import Data.Poly.Internal.Sparse (Poly(..), VPoly, UPoly, leading)
import qualified Data.Poly.Internal.Sparse as Sparse
#if MIN_VERSION_semirings(0,4,2)
import Data.Poly.Internal.Sparse.Field (gcdExt)
import Data.Poly.Internal.Sparse.GcdDomain ()
#endif
#if MIN_VERSION_semirings(0,5,0)
import Data.Euclidean (Field)
#endif

-- | Make 'Poly' from a list of (power, coefficient) pairs.
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [(0,1),(1,2),(2,3)] :: VPoly Integer
-- 3 * X^2 + 2 * X + 1
-- >>> S.toPoly [(0,0),(1,0),(2,0)] :: UPoly Int
-- 0
toPoly :: (Eq a, Semiring a, G.Vector v (Word, a)) => v (Word, a) -> Poly v a
toPoly = Sparse.toPoly'

-- | Create a monomial from a power and a coefficient.
monomial :: (Eq a, Semiring a, G.Vector v (Word, a)) => Word -> a -> Poly v a
monomial = Sparse.monomial'

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^2 + 1) :: UPoly Int
-- 3 * X^4 + 3 * X^2
scale :: (Eq a, Semiring a, G.Vector v (Word, a)) => Word -> a -> Poly v a -> Poly v a
scale = Sparse.scale'

-- | Create an identity polynomial.
pattern X :: (Eq a, Semiring a, G.Vector v (Word, a), Eq (v (Word, a))) => Poly v a
pattern X = Sparse.X'

-- | Evaluate at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
-- >>> eval (X^2 + 1 :: VPoly (UPoly Int)) (X + 1)
-- 1 * X^2 + 2 * X + 2
eval :: (Semiring a, G.Vector v (Word, a)) => Poly v a -> a -> a
eval = Sparse.eval'

-- | Take a derivative.
--
-- >>> deriv (X^3 + 3 * X) :: UPoly Int
-- 3 * X^2 + 3
deriv :: (Eq a, Semiring a, G.Vector v (Word, a)) => Poly v a -> Poly v a
deriv = Sparse.deriv'

#if MIN_VERSION_semirings(0,5,0)
-- | Compute an indefinite integral of a polynomial,
-- setting constant term to zero.
--
-- >>> integral (3 * X^2 + 3) :: UPoly Double
-- 1.0 * X^3 + 3.0 * X
integral :: (Eq a, Field a, G.Vector v (Word, a)) => Poly v a -> Poly v a
integral = Sparse.integral'
#endif
