{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Laurent
  ( testSuite
  ) where

import Prelude hiding (quotRem)
#if MIN_VERSION_semirings(0,4,2)
import Data.Euclidean
#endif
import Data.Int
import Data.Poly.Laurent
import qualified Data.Poly.Laurent.Semiring as S
import Data.Proxy
import Data.Semiring (Semiring)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale, numTests)
import Test.QuickCheck.Classes

import Quaternion
import Sparse (addRef, monomialRef, mulRef, subRef)

instance (Eq a, Semiring a, Arbitrary a, G.Vector v (Int, a)) => Arbitrary (Poly v a) where
  arbitrary = S.toPoly . G.fromList <$> arbitrary
  shrink = fmap (S.toPoly . G.fromList) . shrink . G.toList . unPoly

newtype ShortPoly a = ShortPoly { unShortPoly :: a }
  deriving
    ( Eq
    , Show
    , Semiring
#if MIN_VERSION_semirings(0,4,2)
    , GcdDomain
    , Euclidean
#endif
    )

instance (Eq a, Semiring a, Arbitrary a, G.Vector v (Int, a)) => Arbitrary (ShortPoly (Poly v a)) where
  arbitrary = ShortPoly . S.toPoly . G.fromList . (\xs -> take (length xs `mod` 5) xs) <$> arbitrary
  shrink = fmap (ShortPoly . S.toPoly . G.fromList) . shrink . G.toList . unPoly . unShortPoly

testSuite :: TestTree
testSuite = testGroup "Laurent"
  [ arithmeticTests
  , otherTests
  , lawsTests
  , evalTests
  ]

lawsTests :: TestTree
lawsTests = testGroup "Laws"
  [ semiringTests
  , ringTests
  , numTests
  , euclideanTests
  , isListTests
  , showTests
  ]

semiringTests :: TestTree
semiringTests
  = testGroup "Semiring"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [ semiringLaws (Proxy :: Proxy (Poly U.Vector ()))
  , semiringLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , semiringLaws (Proxy :: Proxy (Poly V.Vector Integer))
  , semiringLaws (Proxy :: Proxy (Poly U.Vector (Quaternion Int)))
  ]

ringTests :: TestTree
ringTests
  = testGroup "Ring"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [
#if MIN_VERSION_quickcheck_classes(0,6,1)
    ringLaws (Proxy :: Proxy (Poly U.Vector ()))
  , ringLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , ringLaws (Proxy :: Proxy (Poly V.Vector Integer))
  , ringLaws (Proxy :: Proxy (Poly U.Vector (Quaternion Int)))
#endif
  ]

numTests :: TestTree
numTests
  = testGroup "Num"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [
#if MIN_VERSION_quickcheck_classes(0,6,3)
    numLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , numLaws (Proxy :: Proxy (Poly V.Vector Integer))
  , numLaws (Proxy :: Proxy (Poly U.Vector (Quaternion Int)))
#endif
  ]

euclideanTests :: TestTree
euclideanTests
  = testGroup "Euclidean"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [
#if MIN_VERSION_semirings(0,4,2) && MIN_VERSION_quickcheck_classes(0,6,3)
    gcdDomainLaws (Proxy :: Proxy (ShortPoly (Poly V.Vector Integer)))
  , euclideanLaws (Proxy :: Proxy (ShortPoly (Poly V.Vector Rational)))
#endif
  ]

isListTests :: TestTree
isListTests
  = testGroup "IsList"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [ isListLaws (Proxy :: Proxy (Poly U.Vector ()))
  , isListLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , isListLaws (Proxy :: Proxy (Poly V.Vector Integer))
  , isListLaws (Proxy :: Proxy (Poly U.Vector (Quaternion Int)))
  ]

showTests :: TestTree
showTests
  = testGroup "Show"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [
#if MIN_VERSION_quickcheck_classes(0,6,0)
    showLaws (Proxy :: Proxy (Poly U.Vector ()))
  , showLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , showLaws (Proxy :: Proxy (Poly V.Vector Integer))
  , showLaws (Proxy :: Proxy (Poly U.Vector (Quaternion Int)))
#endif
  ]

arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic"
  [ testProperty "addition matches reference" $
    \(xs :: [(Int, Int)]) ys -> toPoly (V.fromList (addRef xs ys)) ===
      toPoly (V.fromList xs) + toPoly (V.fromList ys)
  , testProperty "subtraction matches reference" $
    \(xs :: [(Int, Int)]) ys -> toPoly (V.fromList (subRef xs ys)) ===
      toPoly (V.fromList xs) - toPoly (V.fromList ys)
  , testProperty "multiplication matches reference" $
    \(xs :: [(Int, Int)]) ys -> toPoly (V.fromList (mulRef xs ys)) ===
      toPoly (V.fromList xs) * toPoly (V.fromList ys)
  ]

otherTests :: TestTree
otherTests = testGroup "other" $ concat
  [ otherTestGroup (Proxy :: Proxy Int8)
  , otherTestGroup (Proxy :: Proxy (Quaternion Int))
  ]

otherTestGroup
  :: forall a.
     (Eq a, Show a, Semiring a, Num a, Arbitrary a, U.Unbox a, G.Vector U.Vector a)
  => Proxy a
  -> [TestTree]
otherTestGroup _ =
  [ testProperty "leading p 0 == Nothing" $
    \p -> leading (monomial p 0 :: UPoly a) === Nothing
  , testProperty "leading . monomial = id" $
    \p c -> c /= 0 ==> leading (monomial p c :: UPoly a) === Just (p, c)
  , testProperty "monomial matches reference" $
    \p (c :: a) -> monomial p c === toPoly (V.fromList (monomialRef p c))
  , testProperty "scale matches multiplication by monomial" $
    \p c (xs :: UPoly a) -> scale p c xs === monomial p c * xs
  ]

evalTests :: TestTree
evalTests = testGroup "eval" $ concat
  [ evalTestGroup (Proxy :: Proxy (Poly V.Vector Rational))
  ]

evalTestGroup
  :: forall v a.
     (Eq a, Num a, Semiring a, Fractional a, Arbitrary a, Show a, Eq (v (Int, a)), Show (v (Int, a)), G.Vector v (Int, a))
  => Proxy (Poly v a)
  -> [TestTree]
evalTestGroup _ =
  [ testProperty "eval (p + q) r = eval p r + eval q r" $
    \p q r -> e (p + q) r === e p r + e q r
  , testProperty "eval (p * q) r = eval p r * eval q r" $
    \p q r -> e (p * q) r === e p r * e q r
  , testProperty "eval x p = p" $
    \p -> e X p === p
  , testProperty "eval (monomial 0 c) p = c" $
    \c p -> e (monomial 0 c) p === c
  , testProperty "eval' (p + q) r = eval' p r + eval' q r" $
    \p q r -> e' (p + q) r === e' p r + e' q r
  , testProperty "eval' (p * q) r = eval' p r * eval' q r" $
    \p q r -> e' (p * q) r === e' p r * e' q r
  , testProperty "eval' x p = p" $
    \p -> e' S.X p === p
  , testProperty "eval' (S.monomial 0 c) p = c" $
    \c p -> e' (S.monomial 0 c) p === c
  ]
  where
    e :: Poly v a -> a -> a
    e = eval
    e' :: Poly v a -> a -> a
    e' = S.eval
