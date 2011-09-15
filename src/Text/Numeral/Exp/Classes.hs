{-# LANGUAGE NoImplicitPrelude
           , PackageImports
           , TypeSynonymInstances
           , UnicodeSyntax
  #-}

module Text.Numeral.Exp.Classes
    ( Lit(lit)
    , Neg(neg)
    , Add(add)
    , Mul(mul)
    , Sub(sub)
    , Scale(scale)
    ) where

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Prelude ( (+), (*), (^), subtract, negate, fromInteger )
import "base-unicode-symbols" Prelude.Unicode ( ℤ, (⋅) )


-------------------------------------------------------------------------------
-- Exp classes
-------------------------------------------------------------------------------

-- | A literal value.
--
-- Example in English:
--
-- > "three" = lit 3
class Lit α where lit ∷ ℤ → α

-- | Negation of a value.
--
-- Example in English:
--
-- > "minus two" = neg (lit 2)
class Neg α where neg ∷ α → α

-- | Addition of two values.
--
-- Example in English:
--
-- > "fifteen" = lit 5 `add` lit 10
class Add α where add ∷ α → α → α

-- | Multiplication of two values.
--
-- Example in English:
--
-- > "thirty" = lit 3 `mul` lit 10
class Mul α where mul ∷ α → α → α

-- | One value subtracted from another value.
--
-- Example in Latin:
--
-- > "duodēvīgintī" = lit 2 `sub` (lit 2 `mul` lit 10)
class Sub α where sub ∷ α → α → α

-- | A step in a scale of large values.
--
-- Should be interpreted as @10 ^ (rank * base + offset)@.
--
-- Example in English:
--
-- > "quadrillion" = scale 3 3 4
class Scale α where
    scale ∷ ℤ -- ^ Base.
          → ℤ -- ^ Offset.
          → α -- ^ Rank.
          → α

infixl 6 `add`
infixl 6 `sub`
infixl 7 `mul`


-------------------------------------------------------------------------------
-- Integer instances
-------------------------------------------------------------------------------

instance Lit ℤ where lit = fromInteger
instance Neg ℤ where neg = negate
instance Add ℤ where add = (+)
instance Mul ℤ where mul = (*)
instance Sub ℤ where sub = subtract
instance Scale ℤ where scale b o r = 10 ^ (r⋅b + o)
