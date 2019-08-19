-- |
-- Module:     Data.Poly.Internal.Laurent.GcdDomain
-- Copyright:  (c) 2019 Adjoint Inc
-- Licence:    BSD3
-- Maintainer: Adjoint Inc <info@adjoint.io>
--
-- GcdDomain for GcdDomain underlying
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Laurent.GcdDomain
  () where

#if MIN_VERSION_semirings(0,4,2)

import Prelude hiding (gcd, lcm, (^))
import Control.Exception
import Data.Euclidean
import Data.Maybe
import Data.Semiring (Semiring(..))
import qualified Data.Semiring as Semiring
import qualified Data.Vector.Generic as G

import Data.Poly.Internal.Laurent

-- | Consider using 'Data.Poly.Laurent.Semiring.PolyOverFractional' wrapper,
-- which provides a much faster implementation of
-- 'Data.Euclidean.gcd' for 'Fractional'
-- coefficients.
instance (Eq a, Semiring.Ring a, GcdDomain a, Eq (v (Int, a)), G.Vector v (Int, a)) => GcdDomain (Poly v a) where
  divide xs ys = case leading ys of
    Nothing -> throw DivideByZero
    Just (yp, yc) -> case leading xs of
      Nothing -> Just xs
      Just (xp, xc)
        | xp < yp -> Nothing
        | otherwise -> do
          zc <- divide xc yc
          let z = Poly $ G.singleton (xp - yp, zc)
          rest <- divide (xs `plus` Semiring.negate z `times` ys) ys
          pure $ rest `plus` z

  gcd xs ys
    | G.null (unPoly xs) = ys
    | G.null (unPoly ys) = xs
    | otherwise = maybe err (times xy) (divide zs (monomial' 0 (cont zs)))
      where
        err = error "gcd: violated internal invariant"
        zs = gcdHelper xs ys
        cont ts = G.foldl' (\acc (_, t) -> gcd acc t) zero (unPoly ts)
        xy = monomial' 0 (gcd (cont xs) (cont ys))

gcdHelper
  :: (Eq a, Semiring.Ring a, GcdDomain a, G.Vector v (Int, a))
  => Poly v a
  -> Poly v a
  -> Poly v a
gcdHelper xs ys = case leading xs of
  Nothing -> ys
  Just (xp, xc) -> case leading ys of
    Nothing -> xs
    Just (yp, yc) -> case xp `compare` yp of
      LT -> gcdHelper xs (ys `times` monomial' 0 gy `plus` Semiring.negate (xs `times` monomial' (yp - xp) gx))
      EQ -> gcdHelper xs (ys `times` monomial' 0 gy `plus` Semiring.negate (xs `times` monomial' 0 gx))
      GT -> gcdHelper (xs `times` monomial' 0 gx `plus` Semiring.negate (ys `times` monomial' (xp - yp) gy)) ys
      where
        g = lcm xc yc
        gx = fromMaybe err $ divide g xc
        gy = fromMaybe err $ divide g yc
        err = error "gcd: violated internal invariant"

#endif
