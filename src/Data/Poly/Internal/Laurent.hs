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
  , eval
  , deriv
  , integral
  -- * Semiring interface
  , toPoly'
  , monomial'
  , scale'
  , pattern X'
  , eval'
  , deriv'
  ) where

import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.List (intersperse)
import Data.Ord
import Data.Semiring (Semiring(..))
import qualified Data.Semiring as Semiring
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Algorithms.Tim as Tim
import GHC.Exts
#if !MIN_VERSION_semirings(0,4,0)
import Data.Semigroup
import Numeric.Natural
#endif

-- | Polynomials of one variable with coefficients from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
--
-- Use pattern 'X' for construction:
--
-- >>> (X + 1) + (X - 1) :: VPoly Integer
-- 2 * X
-- >>> (X + 1) * (X - 1) :: UPoly Int
-- 1 * X^2 + (-1)
--
-- Polynomials are stored normalized, without
-- zero coefficients, so 0 * 'X' + 1 equals to 1.
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
deriving instance Ord    (v (Int, a)) => Ord    (Poly v a)
deriving instance NFData (v (Int, a)) => NFData (Poly v a)

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
      showCoeff i c = showsPrec 7 c . showString " * X^" . showsPrec 7 i

-- | Polynomials backed by boxed vectors.
type VPoly = Poly V.Vector

-- | Polynomials backed by unboxed vectors.
type UPoly = Poly U.Vector

-- | Make 'Poly' from a list of (power, coefficient) pairs.
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [(0,1),(1,2),(2,3)] :: VPoly Integer
-- 3 * X^2 + 2 * X + 1
-- >>> S.toPoly [(0,0),(1,0),(2,0)] :: UPoly Int
-- 0
toPoly :: (Eq a, Num a, G.Vector v (Int, a)) => v (Int, a) -> Poly v a
toPoly = Poly . normalize (/= 0) (+)

toPoly' :: (Eq a, Semiring a, G.Vector v (Int, a)) => v (Int, a) -> Poly v a
toPoly' = Poly . normalize (/= zero) plus

-- | Return a leading power and coefficient of a non-zero polynomial.
--
-- >>> leading ((2 * X + 1) * (2 * X^2 - 1) :: UPoly Int)
-- Just (3,4)
-- >>> leading (0 :: UPoly Int)
-- Nothing
leading :: G.Vector v (Int, a) => Poly v a -> Maybe (Int, a)
leading (Poly v)
  | G.null v  = Nothing
  | otherwise = Just (G.last v)

normalize
  :: G.Vector v (Int, a)
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (Int, a)
  -> v (Int, a)
normalize p add vs
  | G.null vs = vs
  | otherwise = runST $ do
    ws <- G.thaw vs
    l' <- normalizeM p add ws
    G.unsafeFreeze $ MG.basicUnsafeSlice 0 l' ws

normalizeM
  :: (PrimMonad m, G.Vector v (Int, a))
  => (a -> Bool)
  -> (a -> a -> a)
  -> G.Mutable v (PrimState m) (Int, a)
  -> m Int
normalizeM p add ws = do
    let l = MG.basicLength ws
    let go i j acc@(accP, accC)
          | j >= l = do
            if p accC
              then do
                MG.write ws i acc
                pure $ i + 1
              else pure i
          | otherwise = do
            v@(vp, vc) <- MG.unsafeRead ws j
            if vp == accP
              then go i (j + 1) (accP, accC `add` vc)
              else if p accC
                then do
                  MG.write ws i acc
                  go (i + 1) (j + 1) v
                else go i (j + 1) v
    Tim.sortBy (comparing fst) ws
    wsHead <- MG.unsafeRead ws 0
    go 0 1 wsHead

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

plusPoly
  :: G.Vector v (Int, a)
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (Int, a)
  -> v (Int, a)
  -> v (Int, a)
plusPoly p add xs ys = runST $ do
  zs <- MG.basicUnsafeNew (G.basicLength xs + G.basicLength ys)
  lenZs <- plusPolyM p add xs ys zs
  G.unsafeFreeze $ MG.basicUnsafeSlice 0 lenZs zs
{-# INLINE plusPoly #-}

plusPolyM
  :: (PrimMonad m, G.Vector v (Int, a))
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (Int, a)
  -> v (Int, a)
  -> G.Mutable v (PrimState m) (Int, a)
  -> m Int
plusPolyM p add xs ys zs = go 0 0 0
  where
    lenXs = G.basicLength xs
    lenYs = G.basicLength ys

    go ix iy iz
      | ix == lenXs, iy == lenYs = pure iz
      | ix == lenXs = do
        G.unsafeCopy
          (MG.basicUnsafeSlice iz (lenYs - iy) zs)
          (G.basicUnsafeSlice iy (lenYs - iy) ys)
        pure $ iz + lenYs - iy
      | iy == lenYs = do
        G.unsafeCopy
          (MG.basicUnsafeSlice iz (lenXs - ix) zs)
          (G.basicUnsafeSlice ix (lenXs - ix) xs)
        pure $ iz + lenXs - ix
      | (xp, xc) <- G.unsafeIndex xs ix
      , (yp, yc) <- G.unsafeIndex ys iy
      = case xp `compare` yp of
        LT -> do
          MG.unsafeWrite zs iz (xp, xc)
          go (ix + 1) iy (iz + 1)
        EQ -> do
          let zc = xc `add` yc
          if p zc then do
            MG.unsafeWrite zs iz (xp, zc)
            go (ix + 1) (iy + 1) (iz + 1)
          else
            go (ix + 1) (iy + 1) iz
        GT -> do
          MG.unsafeWrite zs iz (yp, yc)
          go ix (iy + 1) (iz + 1)
{-# INLINE plusPolyM #-}

minusPoly
  :: G.Vector v (Int, a)
  => (a -> Bool)
  -> (a -> a)
  -> (a -> a -> a)
  -> v (Int, a)
  -> v (Int, a)
  -> v (Int, a)
minusPoly p neg sub xs ys = runST $ do
  zs <- MG.basicUnsafeNew (lenXs + lenYs)
  let go ix iy iz
        | ix == lenXs, iy == lenYs = pure iz
        | ix == lenXs = do
          forM_ [iy .. lenYs - 1] $ \i ->
            MG.unsafeWrite zs (iz + i - iy)
              (fmap neg (G.unsafeIndex ys i))
          pure $ iz + lenYs - iy
        | iy == lenYs = do
          G.unsafeCopy
            (MG.basicUnsafeSlice iz (lenXs - ix) zs)
            (G.basicUnsafeSlice ix (lenXs - ix) xs)
          pure $ iz + lenXs - ix
        | (xp, xc) <- G.unsafeIndex xs ix
        , (yp, yc) <- G.unsafeIndex ys iy
        = case xp `compare` yp of
          LT -> do
            MG.unsafeWrite zs iz (xp, xc)
            go (ix + 1) iy (iz + 1)
          EQ -> do
            let zc = xc `sub` yc
            if p zc then do
              MG.unsafeWrite zs iz (xp, zc)
              go (ix + 1) (iy + 1) (iz + 1)
            else
              go (ix + 1) (iy + 1) iz
          GT -> do
            MG.unsafeWrite zs iz (yp, neg yc)
            go ix (iy + 1) (iz + 1)
  lenZs <- go 0 0 0
  G.unsafeFreeze $ MG.basicUnsafeSlice 0 lenZs zs
  where
    lenXs = G.basicLength xs
    lenYs = G.basicLength ys
{-# INLINE minusPoly #-}

scaleM
  :: (PrimMonad m, G.Vector v (Int, a))
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (Int, a)
  -> (Int, a)
  -> G.Mutable v (PrimState m) (Int, a)
  -> m Int
scaleM p mul xs (yp, yc) zs = go 0 0
  where
    lenXs = G.basicLength xs

    go ix iz
      | ix == lenXs = pure iz
      | (xp, xc) <- G.unsafeIndex xs ix
      = do
        let zc = xc `mul` yc
        if p zc then do
          MG.unsafeWrite zs iz (xp + yp, zc)
          go (ix + 1) (iz + 1)
        else
          go (ix + 1) iz
{-# INLINE scaleM #-}

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

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^2 + 1) :: UPoly Int
-- 3 * X^4 + 3 * X^2
scale :: (Eq a, Num a, G.Vector v (Int, a)) => Int -> a -> Poly v a -> Poly v a
scale = scaleInternal (/= 0) (*)

scale' :: (Eq a, Semiring a, G.Vector v (Int, a)) => Int -> a -> Poly v a -> Poly v a
scale' = scaleInternal (/= zero) times

convolution
  :: forall v a.
     G.Vector v (Int, a)
  => (a -> Bool)
  -> (a -> a -> a)
  -> (a -> a -> a)
  -> v (Int, a)
  -> v (Int, a)
  -> v (Int, a)
convolution p add mult xs ys
  | G.basicLength xs >= G.basicLength ys
  = go mult xs ys
  | otherwise
  = go (flip mult) ys xs
  where
    go :: (a -> a -> a) -> v (Int, a) -> v (Int, a) -> v (Int, a)
    go mul long short = runST $ do
      let lenLong   = G.basicLength long
          lenShort  = G.basicLength short
          lenBuffer = lenLong * lenShort
      slices <- MG.basicUnsafeNew lenShort
      buffer <- MG.basicUnsafeNew lenBuffer

      forM_ [0 .. lenShort - 1] $ \iShort -> do
        let (pShort, cShort) = G.unsafeIndex short iShort
            from = iShort * lenLong
            bufferSlice = MG.basicUnsafeSlice from lenLong buffer
        len <- scaleM p mul long (pShort, cShort) bufferSlice
        MG.unsafeWrite slices iShort (from, len)

      slices' <- G.unsafeFreeze slices
      buffer' <- G.unsafeFreeze buffer
      bufferNew <- MG.basicUnsafeNew lenBuffer
      gogo slices' buffer' bufferNew

    gogo
      :: PrimMonad m
      => U.Vector (Int, Int)
      -> v (Int, a)
      -> G.Mutable v (PrimState m) (Int, a)
      -> m (v (Int, a))
    gogo slices buffer bufferNew
      | G.basicLength slices == 0
      = pure G.empty
      | G.basicLength slices == 1
      , (from, len) <- G.unsafeIndex slices 0
      = pure $ G.basicUnsafeSlice from len buffer
      | otherwise = do
        let nSlices = G.basicLength slices
        slicesNew <- MG.basicUnsafeNew ((nSlices + 1) `quot` 2)
        forM_ [0 .. (nSlices - 2) `quot` 2] $ \i -> do
          let (from1, len1) = G.unsafeIndex slices (2 * i)
              (from2, len2) = G.unsafeIndex slices (2 * i + 1)
              slice1 = G.basicUnsafeSlice from1 len1 buffer
              slice2 = G.basicUnsafeSlice from2 len2 buffer
              slice3 = MG.basicUnsafeSlice from1 (len1 + len2) bufferNew
          len3 <- plusPolyM p add slice1 slice2 slice3
          MG.unsafeWrite slicesNew i (from1, len3)

        when (odd nSlices) $ do
          let (from, len) = G.unsafeIndex slices (nSlices - 1)
              slice1 = G.basicUnsafeSlice from len buffer
              slice3 = MG.basicUnsafeSlice from len bufferNew
          G.unsafeCopy slice3 slice1
          MG.unsafeWrite slicesNew (nSlices `quot` 2) (from, len)

        slicesNew' <- G.unsafeFreeze slicesNew
        buffer'    <- G.unsafeThaw   buffer
        bufferNew' <- G.unsafeFreeze bufferNew
        gogo slicesNew' bufferNew' buffer'
{-# INLINE convolution #-}

-- | Create a monomial from a power and a coefficient.
monomial :: (Eq a, Num a, G.Vector v (Int, a)) => Int -> a -> Poly v a
monomial _ 0 = Poly G.empty
monomial p c = Poly $ G.singleton (p, c)

monomial' :: (Eq a, Semiring a, G.Vector v (Int, a)) => Int -> a -> Poly v a
monomial' p c
  | c == zero = Poly G.empty
  | otherwise = Poly $ G.singleton (p, c)

data Strict3 a b c = Strict3 !a !b !c

fst3 :: Strict3 a b c -> a
fst3 (Strict3 a _ _) = a

-- | Evaluate at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
-- >>> eval (X^2 + 1 :: VPoly (UPoly Int)) (X + 1)
-- 1 * X^2 + 2 * X + 2
eval :: (Num a, G.Vector v (Int, a)) => Poly v a -> a -> a
eval (Poly cs) x = fst3 $ G.foldl' go (Strict3 0 0 1) cs
  where
    go (Strict3 acc q xq) (p, c) =
      let xp = xq * x ^ (p - q) in
        Strict3 (acc + c * xp) p xp
{-# INLINE eval #-}

eval' :: (Semiring a, G.Vector v (Int, a)) => Poly v a -> a -> a
eval' (Poly cs) x = fst3 $ G.foldl' go (Strict3 zero 0 one) cs
  where
    go (Strict3 acc q xq) (p, c) =
      let xp = xq `times` (if p == q then one else x Semiring.^ (p - q)) in
        Strict3 (acc `plus` c `times` xp) p xp
{-# INLINE eval' #-}

-- | Take a derivative.
--
-- >>> deriv (X^3 + 3 * X) :: UPoly Int
-- 3 * X^2 + 3
deriv :: (Eq a, Num a, G.Vector v (Int, a)) => Poly v a -> Poly v a
deriv (Poly xs) = Poly $ derivPoly
  (/= 0)
  (\p c -> fromIntegral p * c)
  xs
{-# INLINE deriv #-}

deriv' :: (Eq a, Semiring a, G.Vector v (Int, a)) => Poly v a -> Poly v a
deriv' (Poly xs) = Poly $ derivPoly
  (/= zero)
  (\p c -> fromNatural (fromIntegral p) `times` c)
  xs
{-# INLINE deriv' #-}

#if !MIN_VERSION_semirings(0,4,0)
fromNatural :: Semiring a => Natural -> a
fromNatural 0 = zero
fromNatural n = getAdd' (stimes n (Add' one))

newtype Add' a = Add' { getAdd' :: a }

instance Semiring a => Semigroup (Add' a) where
  Add' a <> Add' b = Add' (a `plus` b)
#endif

derivPoly
  :: G.Vector v (Int, a)
  => (a -> Bool)
  -> (Int -> a -> a)
  -> v (Int, a)
  -> v (Int, a)
derivPoly p mul xs
  | G.null xs = G.empty
  | otherwise = runST $ do
    let lenXs = G.basicLength xs
    zs <- MG.basicUnsafeNew lenXs
    let go ix iz
          | ix == lenXs = pure iz
          | (xp, xc) <- G.unsafeIndex xs ix
          = do
            let zc = xp `mul` xc
            if xp > 0 && p zc then do
              MG.unsafeWrite zs iz (xp - 1, zc)
              go (ix + 1) (iz + 1)
            else
              go (ix + 1) iz
    lenZs <- go 0 0
    G.unsafeFreeze $ MG.basicUnsafeSlice 0 lenZs zs
{-# INLINE derivPoly #-}

-- | Compute an indefinite integral of a polynomial,
-- setting constant term to zero.
--
-- >>> integral (3 * X^2 + 3) :: UPoly Double
-- 1.0 * X^3 + 3.0 * X
integral :: (Eq a, Fractional a, G.Vector v (Int, a)) => Poly v a -> Poly v a
integral (Poly xs)
  = Poly
  $ G.map (\(p, c) -> (p + 1, c / (fromIntegral p + 1))) xs
{-# INLINE integral #-}

-- | Create an identity polynomial.
pattern X :: (Eq a, Num a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a
pattern X <- ((==) var -> True)
  where X = var

var :: forall a v. (Eq a, Num a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a
var
  | (1 :: a) == 0 = Poly G.empty
  | otherwise     = Poly $ G.singleton (1, 1)
{-# INLINE var #-}

-- | Create an identity polynomial.
pattern X' :: (Eq a, Semiring a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a
pattern X' <- ((==) var' -> True)
  where X' = var'

var' :: forall a v. (Eq a, Semiring a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a
var'
  | (one :: a) == zero = Poly G.empty
  | otherwise          = Poly $ G.singleton (1, one)
{-# INLINE var' #-}
