-- |
-- Module:      Data.Poly.Internal.Dense.GcdDomain
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
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
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Dense.GcdDomain
  ( extEuclid
  ) where

#if MIN_VERSION_semirings(0,4,2)

import Prelude hiding (gcd, lcm, (^))
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Euclidean
import Data.Semiring (Semiring(..), isZero, minus)
import qualified Data.Semiring as Semiring
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

import Data.Poly.Internal.Dense

-- | Consider using 'Data.Poly.Semiring.PolyOverFractional' wrapper,
-- which provides a much faster implementation of
-- 'Data.Euclidean.gcd' for 'Fractional'
-- coefficients.
instance (Eq a, Semiring.Ring a, GcdDomain a, Eq (v a), G.Vector v a) => GcdDomain (Poly v a) where
  divide (Poly xs) (Poly ys) =
    toPoly' <$> quotient xs ys

  gcd (Poly xs) (Poly ys)
    | G.null xs = Poly ys
    | G.null ys = Poly xs
    | otherwise = toPoly' $ gcdNonEmpty xs ys
  {-# INLINE gcd #-}

gcdNonEmpty
  :: (Eq a, Semiring.Ring a, GcdDomain a, G.Vector v a)
  => v a
  -> v a
  -> v a
gcdNonEmpty xs ys = runST $ do
    let x = G.foldl1' gcd xs
        y = G.foldl1' gcd ys
        xy = x `gcd` y
    xs' <- G.thaw xs
    ys' <- G.thaw ys
    zs' <- gcdM xs' ys'

    let lenZs = MG.basicLength zs'
        go acc 0 = pure acc
        go acc n = do
          t <- MG.unsafeRead zs' (n - 1)
          go (acc `gcd` t) (n - 1)
    a <- MG.unsafeRead zs' (lenZs - 1)
    z <- go a (lenZs - 1)

    let err = error "gcdNonEmpty: violated internal invariant"
    forM_ [0 .. lenZs - 1] $ \i ->
      MG.unsafeModify
        zs'
        (\c -> maybe err (`times` xy) (c `divide` z))
        i

    G.unsafeFreeze zs'

gcdM
  :: (PrimMonad m, Eq a, Semiring.Ring a, GcdDomain a, G.Vector v a)
  => G.Mutable v (PrimState m) a
  -> G.Mutable v (PrimState m) a
  -> m (G.Mutable v (PrimState m) a)
gcdM xs ys
  | MG.null xs = pure ys
  | MG.null ys = pure xs
  | otherwise = do
  let lenXs = MG.basicLength xs
      lenYs = MG.basicLength ys
  xLast <- MG.unsafeRead xs (lenXs - 1)
  yLast <- MG.unsafeRead ys (lenYs - 1)
  let z = xLast `lcm` yLast
      zx = case z `divide` xLast of
        Nothing -> error "gcdM: highest coefficient is 0"
        Just t  -> t
      zy = case z `divide` yLast of
        Nothing -> error "gcdM: highest coefficient is 0"
        Just t  -> t

  if lenXs <= lenYs then do
    forM_ [0 .. lenXs - 1] $ \i -> do
      x <- MG.unsafeRead xs i
      MG.unsafeModify
        ys
        (\y -> (y `times` zy) `plus` Semiring.negate (x `times` zx))
        (i + lenYs - lenXs)
    forM_ [0 .. lenYs - lenXs - 1] $
      MG.unsafeModify ys (`times` zy)
    ys' <- dropWhileEndM isZero ys
    gcdM xs ys'
  else do
    forM_ [0 .. lenYs - 1] $ \i -> do
      y <- MG.unsafeRead ys i
      MG.unsafeModify
        xs
        (\x -> (x `times` zx) `plus` Semiring.negate (y `times` zy))
        (i + lenXs - lenYs)
    forM_ [0 .. lenXs - lenYs - 1] $
      MG.unsafeModify xs (`times` zx)
    xs' <- dropWhileEndM isZero xs
    gcdM xs' ys
{-# INLINE gcdM #-}

isZeroM
  :: (Eq a, Semiring a, PrimMonad m, G.Vector v a)
  => G.Mutable v (PrimState m) a
  -> m Bool
isZeroM xs = go (MG.basicLength xs)
  where
    go 0 = pure True
    go n = do
      x <- MG.unsafeRead xs (n - 1)
      if x == zero then go (n - 1) else pure False
{-# INLINE isZeroM #-}

quotient
  :: (Eq a, Eq (v a), Semiring.Ring a, GcdDomain a, G.Vector v a)
  => v a
  -> v a
  -> Maybe (v a)
quotient xs ys
  | G.null ys = throw DivideByZero
  | G.null xs = Just xs
  | G.basicLength xs < G.basicLength ys = Nothing
  | otherwise = runST $ do
    let lenXs = G.basicLength xs
        lenYs = G.basicLength ys
        lenQs = lenXs - lenYs + 1
    qs <- MG.basicUnsafeNew lenQs
    rs <- MG.basicUnsafeNew lenXs
    G.unsafeCopy rs xs

    let go i
          | i < 0 = do
            b <- isZeroM rs
            if b
              then Just <$> G.unsafeFreeze qs
              else pure Nothing
          | otherwise = do
            r <- MG.unsafeRead rs (lenYs - 1 + i)
            case r `divide` G.unsafeLast ys of
              Nothing -> pure Nothing
              Just q -> do
                MG.unsafeWrite qs i q
                forM_ [0 .. lenYs - 1] $ \k -> do
                  MG.unsafeModify
                    rs
                    (\c -> c `plus` (Semiring.negate $ q `times` G.unsafeIndex ys k))
                    (i + k)
                go (i - 1)

    go (lenQs - 1)
{-# INLINE quotient #-}

-- | Extended Euclidean algorithm.
extEuclid
  :: (Eq (v a), Euclidean (Poly v a), GcdDomain (Poly v a), Semiring.Ring (Poly v a))
  => Poly v a
  -> Poly v a
  -> (Poly v a, (Poly v a, Poly v a))
extEuclid xs ys = go ys xs zero one one zero
  where
    go r r' s s' t t'
      | r == zero = (r', (s', t'))
      | otherwise = case Data.Euclidean.quot r' r of
        q -> go (r' `minus` q `times` r) r
                (s' `minus` q `times` s) s
                (t' `minus` q `times` t) t
{-# INLINE extEuclid #-}

#endif
