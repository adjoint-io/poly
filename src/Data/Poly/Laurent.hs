-- |
-- Module:     Data.Poly.Laurent
-- Copyright:  (c) 2019 Adjoint Inc
-- Licence:    BSD3
-- Maintainer: Adjoint Inc <info@adjoint.io>
--
-- Laurent polynomials with 'Num' instance.
--

{-# LANGUAGE PatternSynonyms #-}

module Data.Poly.Laurent
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  , leading
  -- * Num interface
  , toPoly
  , monomial
  , scale
  , pattern X
  , (^-)
  -- * Fractional coefficients
  , eval
  ) where

import Data.Poly.Internal.Laurent
import Data.Poly.Internal.Laurent.Fractional (eval)
import Data.Poly.Internal.Laurent.GcdDomain ()
