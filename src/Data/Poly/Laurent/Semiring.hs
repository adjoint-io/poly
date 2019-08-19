-- |
-- Module:     Data.Poly.Laurent.Semiring
-- Copyright:  (c) 2019 Adjoint Inc
-- Licence:    BSD3
-- Maintainer: Adjoint Inc <info@adjoint.io>
--
-- Laurent polynomials with 'Semiring' instance.
--

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

module Data.Poly.Laurent.Semiring
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
  , (^-)
  -- , eval
  ) where

import Data.Semiring (Semiring)
import Data.Vector.Generic (Vector)

import Data.Poly.Internal.Laurent (Poly(..), VPoly, UPoly, leading, (-^))
import qualified Data.Poly.Internal.Laurent as Laurent
import Data.Poly.Internal.Laurent.Fractional ()
import Data.Poly.Internal.Laurent.GcdDomain ()

-- | Make 'Poly' from a list of (power, coefficient) pairs.
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [(0,1),(-1,2),(-2,3)] :: VPoly Integer
-- 1 + 2 * X^-1 + 3 * X^-2
-- >>> toPoly [(0,0),(-1,0),(-2,0)] :: UPoly Int
-- 0
toPoly :: (Eq a, Semiring a, Vector v (Int, a)) => v (Int, a) -> Poly v a
toPoly = Laurent.toPoly'

-- | Create a monomial from a power and a coefficient.
monomial :: (Eq a, Semiring a, Vector v (Int, a)) => Int -> a -> Poly v a
monomial = Laurent.monomial'

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale (-2) 3 (X^-2 + 1) :: UPoly Int
-- 3 * X^-2 + 3 * X^-4
scale :: (Eq a, Semiring a, Vector v (Int, a)) => Int -> a -> Poly v a -> Poly v a
scale = Laurent.scale'

-- | Create an identity polynomial.
pattern X :: (Eq a, Semiring a, Vector v (Int, a), Eq (v (Int, a))) => Poly v a
pattern X = Laurent.X'

-- | Exponentiation of an identity Laurent polynomial to a negative integer.
infixr 8 ^-
(^-) :: (Eq a, Semiring a, Vector v (Int, a), Eq (v (Int, a))) => Poly v a -> Int -> Poly v a
(^-) = (-^)

-- | Evaluate at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
-- >>> eval (X^2 + 1 :: VPoly (UPoly Int)) (X + 1)
-- 1 * X^2 + 2 * X + 2
-- eval :: (Semiring a, Vector v (Int, a)) => Poly v a -> a -> a
-- eval = Laurent.eval'
