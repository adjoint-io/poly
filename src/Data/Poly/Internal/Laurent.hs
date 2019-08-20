-- |
-- Module:     Data.Poly.Internal.Laurent
-- Copyright:  (c) 2019 Adjoint Inc
-- Licence:    BSD3
-- Maintainer: Adjoint Inc <info@adjoint.io>
--
-- Laurent polynomials of one variable.
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.Poly.Internal.Laurent
  ( Poly(..)
  , VPoly
  , UPoly
  , leading
  -- * Num interface
  , toPoly
  , monomial
  , scale
  , pattern X
  , (^-)
  -- * Semiring interface
  , toPoly'
  , monomial'
  , scale'
  , pattern X'
  , (-^)
  ) where

import Control.DeepSeq (NFData)
import Control.Monad.ST (runST)
import Data.List (intersperse)
import Data.Poly.Internal.Sparse (convolution, minusPoly, normalize, plusPoly, scaleM)
import Data.Semiring (Semiring(..))
import qualified Data.Semiring as Semiring (Ring(..))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed as U
import GHC.Exts (IsList(..))
#if !MIN_VERSION_semirings(0,4,0)
import Data.Semigroup
import Numeric.Natural
#endif

-- | Laurent polynomials of one variable with coefficients from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
--
-- Use pattern 'X' for construction:
--
-- >>> (X + 1) + (X - 1) :: VPoly Integer
-- 2 * X
-- >>> (X + 1) * (X - 1) :: UPoly Int
-- 1 * X^2 + (-1)
--
-- Use '^-' to invert pattern 'X':
--
-- >>> X^-1 :: UPoly Int
-- 1 * X^-1
--
-- Laurent polynomials are stored normalized, without
-- zero coefficients, so 0 * 'X^-1' + 1 equals to 1.
--
-- 'Ord' instance does not make much sense mathematically,
-- it is defined only for the sake of 'Data.Set.Set', 'Data.Map.Map', etc.
--
newtype Poly v a = Poly
  { unPoly :: v (Int, a)
  -- ^ Convert 'Poly' to a vector of coefficients
  -- (first element corresponds to a constant term).
  }

deriving instance Eq     (v (Int, a)) => Eq     (Poly v a)
deriving instance NFData (v (Int, a)) => NFData (Poly v a)
deriving instance Ord    (v (Int, a)) => Ord    (Poly v a)

instance (Eq a, Semiring a, G.Vector v (Int, a)) => IsList (Poly v a) where
  type Item (Poly v a) = (Int, a)
  fromList = toPoly' . G.fromList
  fromListN = (toPoly' .) . G.fromListN
  toList = G.toList . unPoly

instance (Show a, G.Vector v (Int, a)) => Show (Poly v a) where
  showsPrec d (Poly xs)
    | G.null xs
      = showString "0"
    | otherwise
      = showParen (d > 0)
      $ foldl (.) id
      $ intersperse (showString " + ")
      $ G.foldl (\acc (i, c) -> showCoeff i c : acc) [] xs
    where
      showCoeff 0 c = showsPrec 7 c
      showCoeff 1 c = showsPrec 7 c . showString " * X"
      showCoeff i c = showsPrec 7 c . showString " * X^" . showsPrec 6 i

-- | Laurent polynomials backed by boxed vectors.
type VPoly = Poly V.Vector

-- | Laurent polynomials backed by unboxed vectors.
type UPoly = Poly U.Vector

-- | Make 'Poly' from a list of (power, coefficient) pairs.
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [(0,1),(-1,2),(-2,3)] :: VPoly Integer
-- 1 + 2 * X^-1 + 3 * X^-2
-- >>> toPoly [(0,0),(-1,0),(-2,0)] :: UPoly Int
-- 0
toPoly :: (Eq a, Num a, G.Vector v (Int, a)) => v (Int, a) -> Poly v a
toPoly = Poly . normalize (/= 0) (+)

toPoly' :: (Eq a, Semiring a, G.Vector v (Int, a)) => v (Int, a) -> Poly v a
toPoly' = Poly . normalize (/= zero) plus

-- | Return a leading power and coefficient of a non-zero Laurent polynomial.
--
-- >>> leading ((2 * X^-1 + 1) * (2 * X^-2 - 1) :: UPoly Int)
-- Just (0,-1)
-- >>> leading (0 :: UPoly Int)
-- Nothing
leading :: G.Vector v (Int, a) => Poly v a -> Maybe (Int, a)
leading (Poly v)
  | G.null v  = Nothing
  | otherwise = Just (G.last v)

-- | Note that 'abs' = 'id' and 'signum' = 'const' 1.
instance (Eq a, Num a, G.Vector v (Int, a)) => Num (Poly v a) where
  Poly xs + Poly ys = Poly $ plusPoly (/= 0) (+) xs ys
  Poly xs - Poly ys = Poly $ minusPoly (/= 0) negate (-) xs ys
  negate (Poly xs) = Poly $ G.map (fmap negate) xs
  abs = id
  signum = const 1
  fromInteger n = case fromInteger n of
    0 -> Poly $ G.empty
    m -> Poly $ G.singleton (0, m)
  Poly xs * Poly ys = Poly $ convolution (/= 0) (+) (*) xs ys
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}
  {-# INLINE (*) #-}

instance (Eq a, Semiring a, G.Vector v (Int, a)) => Semiring (Poly v a) where
  zero = Poly G.empty
  one
    | (one :: a) == zero = zero
    | otherwise = Poly $ G.singleton (0, one)
  plus (Poly xs) (Poly ys) = Poly $ plusPoly (/= zero) plus xs ys
  times (Poly xs) (Poly ys) = Poly $ convolution (/= zero) plus times xs ys
  {-# INLINE zero #-}
  {-# INLINE one #-}
  {-# INLINE plus #-}
  {-# INLINE times #-}

#if MIN_VERSION_semirings(0,4,0)
  fromNatural n = if n' == zero then zero else Poly $ G.singleton (0, n')
    where
      n' :: a
      n' = fromNatural n
  {-# INLINE fromNatural #-}
#endif

instance (Eq a, Semiring.Ring a, G.Vector v (Int, a)) => Semiring.Ring (Poly v a) where
  negate (Poly xs) = Poly $ G.map (fmap Semiring.negate) xs

scaleInternal
  :: G.Vector v (Int, a)
  => (a -> Bool)
  -> (a -> a -> a)
  -> Int
  -> a
  -> Poly v a
  -> Poly v a
scaleInternal p mul yp yc (Poly xs) = runST $ do
  zs <- MG.basicUnsafeNew (G.basicLength xs)
  len <- scaleM p (flip mul) xs (yp, yc) zs
  fmap Poly $ G.unsafeFreeze $ MG.basicUnsafeSlice 0 len zs
{-# INLINE scaleInternal #-}

-- | Multiply a Laurent polynomial by a Laurent monomial, expressed as a power and a coefficient.
--
-- >>> scale (-2) 3 (X^-2 + 1) :: UPoly Int
-- 3 * X^-2 + 3 * X^-4
scale :: (Eq a, Num a, G.Vector v (Int, a)) => Int -> a -> Poly v a -> Poly v a
scale = scaleInternal (/= 0) (*)

scale' :: (Eq a, Semiring a, G.Vector v (Int, a)) => Int -> a -> Poly v a -> Poly v a
scale' = scaleInternal (/= zero) times

-- | Create a Laurent monomial from a power and a coefficient.
monomial :: (Eq a, Num a, G.Vector v (Int, a)) => Int -> a -> Poly v a
monomial _ 0 = Poly G.empty
monomial p c = Poly $ G.singleton (p, c)

monomial' :: (Eq a, Semiring a, G.Vector v (Int, a)) => Int -> a -> Poly v a
monomial' p c
  | c == zero = Poly G.empty
  | otherwise = Poly $ G.singleton (p, c)

#if !MIN_VERSION_semirings(0,4,0)
fromNatural :: Semiring a => Natural -> a
fromNatural 0 = zero
fromNatural n = getAdd' (stimes n (Add' one))

newtype Add' a = Add' { getAdd' :: a }

instance Semiring a => Semigroup (Add' a) where
  Add' a <> Add' b = Add' (a `plus` b)
#endif

-- | Create an identity polynomial.
pattern X :: (Eq a, Num a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a
pattern X <- ((==) var -> True)
  where X = var

var :: forall a v. (Eq a, Num a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a
var
  | (1 :: a) == 0 = Poly G.empty
  | otherwise     = Poly $ G.singleton (1, 1)
{-# INLINE var #-}

pattern X' :: (Eq a, Semiring a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a
pattern X' <- ((==) var' -> True)
  where X' = var'

var' :: forall a v. (Eq a, Semiring a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a
var'
  | (one :: a) == zero = Poly G.empty
  | otherwise          = Poly $ G.singleton (1, one)
{-# INLINE var' #-}

-- | Exponentiation of an identity Laurent polynomial to a negative integer.
infixr 8 ^-
(^-) :: (Eq a, Num a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a -> Int -> Poly v a
X ^- n = monomial (-n) 1
_ ^- _ = error "(^-): not the identity Laurent polynomial"
{-# INLINE (^-) #-}

infixr 8 -^
(-^) :: (Eq a, Semiring a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a -> Int -> Poly v a
X' -^ n = monomial' (-n) one
_  -^ _ = error "(-^): not the identity Laurent polynomial"
{-# INLINE (-^) #-}
