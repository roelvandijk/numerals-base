{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , PackageImports
           , RecordWildCards
  #-}

module Text.Numeral.Render
    ( -- * Rendering numerals
      render
      -- * Representation of numerals
    , Repr(..)
    , ScaleRepr
    , defaultRepr
      -- * Context of expressions
    , Ctx(..)
    , isOutside
    )
    where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Bool     ( Bool(False, True), otherwise )
import "base" Data.Function ( ($) )
import "base" Data.Functor  ( (<$>) )
import "base" Data.Maybe    ( Maybe(Nothing, Just) )
import "base" Data.Monoid   ( Monoid )
import "base" Text.Show     ( Show )
import "base-unicode-symbols" Data.Eq.Unicode     ( (≡) )
import "base-unicode-symbols" Data.Monoid.Unicode ( (⊕) )
import "base-unicode-symbols" Prelude.Unicode     ( ℤ )
import "this"                 Text.Numeral.Exp    ( Exp(..), Side(L, R) )


-------------------------------------------------------------------------------
-- Rendering numerals
-------------------------------------------------------------------------------

-- | Renders an expression to a string-like value according to a
-- certain representation.
render ∷ (Monoid s) ⇒ Repr s → Exp → Maybe s
render (Repr {..}) e = go CtxEmpty e
    where
      go _   Unknown = reprUnknown
      go ctx (Lit n) = ($ ctx) <$> reprValue n
      go ctx (Scale b o r) = reprScale b o r ctx
      go ctx (Neg x) = do x' ← go (CtxNeg ctx) x
                          rn ← reprNeg
                          rnc ← reprNegCombine
                          Just $ rnc (rn x ctx) x'
      go ctx (Add x y) = do x' ← go (CtxAdd L y ctx) x
                            y' ← go (CtxAdd R x ctx) y
                            ra ← reprAdd
                            rac ← reprAddCombine
                            Just $ rac (ra x y ctx) x' y'
      go ctx (Mul x y) = do x' ← go (CtxMul L y ctx) x
                            y' ← go (CtxMul R x ctx) y
                            rm ← reprMul
                            rmc ← reprMulCombine
                            Just $ rmc (rm x y ctx) x' y'
      go ctx (Sub x y) = do x' ← go (CtxSub L y ctx) x
                            y' ← go (CtxSub R x ctx) y
                            rs ← reprSub
                            rsc ← reprSubCombine
                            Just $ rsc (rs x y ctx) x' y'


--------------------------------------------------------------------------------
-- Representation of numerals
--------------------------------------------------------------------------------

-- | A representation for numerals.
--
-- A 'Repr' contains all the information on how to render an
-- 'Exp'ression to a string-like value.
data Repr s =
    Repr
    { -- | Representation for unknown values.
      reprUnknown ∷ Maybe s
      -- | Renders a literal value. Not necessarily defined for every
      -- value.
    , reprValue ∷ ℤ → Maybe (Ctx Exp → s)
      -- | Renders a step in a scale of large values.
    , reprScale ∷ ScaleRepr s
      -- | Renders a negation. This concerns the negation itself, not
      -- the thing being negated.
    , reprNeg ∷ Maybe (Exp       → Ctx Exp → s)
      -- | Renders an addition. This concerns the addition itself, not
      -- the things being added. For example: In \"one hundred and
      -- eighty\" this function would be responsible for rendering the
      -- \"and\".
    , reprAdd ∷ Maybe (Exp → Exp → Ctx Exp → s)
      -- | Renders a multiplication. This concerns the multiplication
      -- itself, not the things being multiplied.
    , reprMul ∷ Maybe (Exp → Exp → Ctx Exp → s)
      -- | Renders a subtraction. This concerns the subtraction
      -- itself, not the things being subtracted.
    , reprSub ∷ Maybe (Exp → Exp → Ctx Exp → s)
      -- | Combines a negation and the thing being negated. For
      -- example: this would combine \"minus\" and \"three\" into
      -- \"minus three\".
    , reprNegCombine ∷ Maybe (s → s     → s)
      -- | Combines an addition and the things being added.
    , reprAddCombine ∷ Maybe (s → s → s → s)
      -- | Combines a multiplication and the things being multiplied.
    , reprMulCombine ∷ Maybe (s → s → s → s)
      -- | Combines a subtraction and the things being subtracted.
    , reprSubCombine ∷ Maybe (s → s → s → s)
    }

-- | Function that renders the representation of a step in a scale of
-- large values. The value represented by the step is 10 ^ (rank *
-- base + offset).
type ScaleRepr s = ℤ -- ^ Base.
                 → ℤ -- ^ Offset.
                 → Exp -- ^ Rank.
                 → Ctx Exp -- ^ Rank context.
                 → Maybe s

-- | The default representation.
--
-- Only the combining functions are defined. The rest are either
-- 'Nothing' or always produce 'Nothing'.
defaultRepr ∷ (Monoid s) ⇒ Repr s
defaultRepr =
    Repr { reprUnknown = Nothing
         , reprValue = \_       → Nothing
         , reprScale = \_ _ _ _ → Nothing
         , reprNeg   = Nothing
         , reprAdd   = Nothing
         , reprMul   = Nothing
         , reprSub   = Nothing
         , reprNegCombine = Just $ \n x   → n ⊕ x
         , reprAddCombine = Just $ \a x y → x ⊕ a ⊕ y
         , reprMulCombine = Just $ \m x y → x ⊕ m ⊕ y
         , reprSubCombine = Just $ \s x y → x ⊕ s ⊕ y
         }


--------------------------------------------------------------------------------
-- Context of expressions
--------------------------------------------------------------------------------

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

-- | Checks whether a context is completely on the outside of an
-- expression, either left or right.
--
-- Given the following expression:
--
-- @
-- 'Add' ('Lit' 1000) ('Add' ('Mul' ('Lit' 2) ('Lit' 100)) ('Add' ('Lit' 4) ('Mul' ('Lit' 3) ('Lit' 10))))
-- @
--
-- On the left we have @'Lit' 1000@ and on the right @'Lit' 10@.
isOutside ∷ Side → Ctx α → Bool
isOutside s c = go c
    where
      go ∷ Ctx α → Bool
      go CtxEmpty = True
      go (CtxNeg nc) = go nc
      go (CtxAdd as _ ac) | as ≡ s = go ac
                          | otherwise = False
      go (CtxMul ms _ mc) | ms ≡ s = go mc
                          | otherwise = False
      go (CtxSub ss _ sc) | ss ≡ s = go sc
                          | otherwise = False
      go (CtxScale sc) = go sc
