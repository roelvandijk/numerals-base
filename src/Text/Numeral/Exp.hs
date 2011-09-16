{-# LANGUAGE NoImplicitPrelude
           , PackageImports
           , UnicodeSyntax
  #-}

module Text.Numeral.Exp
    ( Exp(..)
    , eval
    , Side(L, R)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool ( Bool(False, True) )
import "base" Data.Eq   ( Eq )
import "base" Data.Ord  ( Ord )
import "base" Text.Show ( Show )
import "base-unicode-symbols" Prelude.Unicode ( ℤ )
import qualified "this" Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- Exp datatype
-------------------------------------------------------------------------------

-- | An expression that represents the structure of a numeral.
data Exp  -- | An unknown value.
         = Unknown
          -- | A literal value.
         | Lit ℤ
           -- | Negation of an expression.
         | Neg Exp
           -- | Addition of two expressions.
         | Add Exp Exp
           -- | Multiplication of two expressions.
         | Mul Exp Exp
           -- | One expression subtracted from another expression.
         | Sub Exp Exp
           -- | A step in a scale of large values.
         | Scale ℤ ℤ Exp
           deriving (Eq, Ord, Show)

infixl 6 `Add`
infixl 6 `Sub`
infixl 7 `Mul`

-- | Precisely the 'Unknown' constructor.
instance C.Unknown Exp where
    unknown = Unknown
    isUnknown Unknown = True
    isUnknown _       = False
-- | Precisely the 'Lit' constructor.
instance C.Lit Exp where lit = Lit
-- | Precisely the 'Neg' constructor.
instance C.Neg Exp where neg = Neg
-- | Precisely the 'Add' constructor.
instance C.Add Exp where add = Add
-- | Precisely the 'Mul' constructor.
instance C.Mul Exp where mul = Mul
-- | Precisely the 'Sub' constructor.
instance C.Sub Exp where sub = Sub
-- | Precisely the 'Scale' constructor.
instance C.Scale Exp where scale = Scale

-- | Evaluates an expression to a value.
--
-- Law: @e == eval e@
eval ∷ (C.Unknown α, C.Lit α, C.Neg α, C.Add α, C.Mul α, C.Sub α, C.Scale α) ⇒ Exp → α
eval (Add x y)     = C.add (eval x) (eval y)
eval (Mul x y)     = C.mul (eval x) (eval y)
eval (Sub x y)     = C.sub (eval x) (eval y)
eval (Neg x)       = C.neg (eval x)
eval (Lit x)       = C.lit x
eval (Scale b o r) = C.scale b o (eval r)
eval Unknown       = C.unknown

-- prop_eval ∷ Exp → Bool
-- prop_eval e = e ≡ eval e


-------------------------------------------------------------------------------
-- Side
-------------------------------------------------------------------------------

-- | A side or direction, either 'L'eft or 'R'ight.
data Side = L -- ^ Left.
          | R -- ^ Right.
            deriving (Eq, Show)
