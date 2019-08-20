-- |
-- Module:     Data.Poly.Internal.Laurent.Fractional
-- Copyright:  (c) 2019 Adjoint Inc
-- Licence:    BSD3
-- Maintainer: Adjoint Inc <info@adjoint.io>
--
-- GcdDomain for Fractional underlying
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

#if MIN_VERSION_semirings(0,4,2)

module Data.Poly.Internal.Laurent.Fractional
  ( eval
  , fractionalGcd
  ) where

import Prelude
import Control.Arrow (first)
import Control.Exception (ArithException(..), throw)
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import qualified Data.Semiring as Semiring (Ring)
import qualified Data.Vector.Generic as G

import Data.Poly.Internal.Laurent
import Data.Poly.Internal.Laurent.GcdDomain ()

instance (Eq a, Eq (v (Int, a)), Semiring.Ring a, GcdDomain a, Fractional a, G.Vector v (Int, a)) => Euclidean (Poly v a) where
  degree (Poly xs)
    | G.null xs = 0
    | otherwise = 1 + fromIntegral (fst (G.last xs))

  quotRem = quotientRemainder

quotientRemainder
  :: (Eq a, Fractional a, G.Vector v (Int, a))
  => Poly v a
  -> Poly v a
  -> (Poly v a, Poly v a)
quotientRemainder ts ys = case leading ys of
  Nothing -> throw DivideByZero
  Just (yp, yc) -> go ts
    where
      go xs = case leading xs of
        Nothing -> (0, 0)
        Just (xp, xc) -> case xp `compare` yp of
          LT -> (0, xs)
          EQ -> (zs, xs')
          GT -> first (+ zs) $ go xs'
          where
            zs = Poly $ G.singleton (xp - yp, xc / yc)
            xs' = xs - zs * ys

fractionalGcd
  :: (Eq a, Fractional a, G.Vector v (Int, a))
  => Poly v a
  -> Poly v a
  -> Poly v a
fractionalGcd xs ys
  | G.null (unPoly ys) = xs
  | otherwise = fractionalGcd ys $ snd $ quotientRemainder xs ys
{-# INLINE fractionalGcd #-}

data Strict3 a b c = Strict3 !a !b !c

fst3 :: Strict3 a b c -> a
fst3 (Strict3 a _ _) = a

-- | Evaluate at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
-- >>> eval (X^2 + 1 :: VPoly (UPoly Int)) (X + 1)
-- 1 * X^2 + 2 * X + 2
eval :: (Fractional a, G.Vector v (Int, a)) => Poly v a -> a -> a
eval (Poly cs) x = fst3 $ G.foldl' go (Strict3 0 0 1) cs
  where
    go (Strict3 acc q xq) (p, c) = let xp = xq * pow (p - q)
      in Strict3 (acc + c * xp) p xp
      where
        pow n = if n < 0 then recip x ^ negate n else x ^ n
{-# INLINE eval #-}

#else

module Data.Poly.Internal.Laurent.Fractional () where

#endif
