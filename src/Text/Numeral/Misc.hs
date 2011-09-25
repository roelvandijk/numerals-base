{-# LANGUAGE MagicHash
           , NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

module Text.Numeral.Misc where

--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Function ( ($) )
import "base" GHC.Exts      ( Int(I#) )
import "base" Prelude       ( Integral, fromIntegral, toInteger, (^) )
import "integer-gmp" GHC.Integer.Logarithms ( integerLogBase# )

--------------------------------------------------------------------------------
-- Misc
--------------------------------------------------------------------------------

-- ^ Raise 10 to some power.
dec ∷ (Integral α) ⇒ α → α
dec = (10 ^)

-- ^ The (base 10) logarithm of an integral value. Note that the
-- result must be able to fit in an ordinary Int value. This means the
-- maximum input value is 10 ^ (maxBound ∷ Int).
intLog ∷ (Integral α) ⇒ α → α
intLog x = fromIntegral $ I# $ integerLogBase# 10 (toInteger x)
