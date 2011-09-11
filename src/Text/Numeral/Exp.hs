{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Text.Numeral.Exp
    ( Exp(..)
    , eval
    , Side(L, R)
    , Ctx(..)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Eq   ( Eq )
import Data.Ord  ( Ord )
import Text.Show ( Show )

-- from base-unicode-symbols:
import Prelude.Unicode ( ℤ )

-- from numerals-base:
import qualified Text.Numeral.Exp.Classes as C


-------------------------------------------------------------------------------
-- Exp datatype
-------------------------------------------------------------------------------

-- | An expression that represents the structure of a numeral.
data Exp   -- | A literal value.
         = Lit ℤ
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
eval ∷ (C.Lit α, C.Neg α, C.Add α, C.Mul α, C.Sub α, C.Scale α) ⇒ Exp → α
eval (Add x y)     = C.add (eval x) (eval y)
eval (Mul x y)     = C.mul (eval x) (eval y)
eval (Sub x y)     = C.sub (eval x) (eval y)
eval (Neg x)       = C.neg (eval x)
eval (Lit x)       = C.lit x
eval (Scale b o r) = C.scale b o (eval r)

-- prop_eval ∷ Exp → Bool
-- prop_eval e = e ≡ eval e


-------------------------------------------------------------------------------
-- Side
-------------------------------------------------------------------------------

-- | A side or direction, either 'L'eft or 'R'ight.
data Side = L -- ^ Left.
          | R -- ^ Right.
            deriving Show


-------------------------------------------------------------------------------
-- Context of expressions
-------------------------------------------------------------------------------

-- | A context in which an 'Exp'ression appears.
data Ctx α   -- | The empty context. Used for top level expressions.
           = CtxEmpty
             -- | Negation context.
           | CtxNeg (Ctx α)
             -- | Addition context.
           | CtxAdd Side α (Ctx α)
             -- | Multiplication context.
           | CtxMul Side α (Ctx α)
             -- | Subtraction context.
           | CtxSub Side α (Ctx α)
             -- | Scale context.
           | CtxScale (Ctx α)
             deriving Show
