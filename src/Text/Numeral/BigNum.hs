{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , PackageImports
           , UnicodeSyntax
  #-}

module Text.Numeral.BigNum
  ( cardinal
  , rule
  , cardinalRepr
  , symMap
  , forms

  , scaleRepr
  , pelletierRepr
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base"                       Data.Bool             ( otherwise )
import "base"                       Data.Function         ( ($), const, fix )
import "base"                       Data.Functor          ( (<$>) )
import "base"                       Data.Maybe            ( Maybe(Nothing, Just) )
import "base"                       Data.Monoid           ( Monoid )
import "base"                       Data.String           ( IsString )
import "base"                       Prelude               ( Integral )
import "base-unicode-symbols"       Data.Eq.Unicode       ( (≡) )
import "base-unicode-symbols"       Data.Function.Unicode ( (∘) )
import "base-unicode-symbols"       Data.List.Unicode     ( (∈) )
import "base-unicode-symbols"       Data.Monoid.Unicode   ( (⊕) )
import "base-unicode-symbols"       Prelude.Unicode       ( ℤ )
import "containers-unicode-symbols" Data.Map.Unicode      ( (∪) )
import "this"                       Text.Numeral
import qualified "containers" Data.Map as M ( Map, fromList, lookup )
import qualified "this"       Text.Numeral.Exp.Classes as C


--------------------------------------------------------------------------------
-- Language of Big Numbers
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = render cardinalRepr ∘ (pos $ fix rule)

rule ∷ (Integral α, C.Unknown β, C.Lit β, C.Add β, C.Mul β) ⇒ Rule α β
rule = findRule (   1, lit        )
              [ (  11, add  10 L  )
              , (  20, mul  10 L L)
              , ( 100, lit        )
              , ( 101, add 100 L  )
              , ( 200, mul 100 R L)
              , (1000, lit        )
              ]
                 1000

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr s
cardinalRepr =
    defaultRepr { reprValue = \n → M.lookup n symMap
                , reprAdd   = Just $ \_ _ _ → ""
                , reprMul   = Just $ \_ _ _ → ""
                }

symMap ∷ (Integral α, IsString s) ⇒ M.Map α (Ctx Exp → s)
symMap = M.fromList
         [ (1, forms "m"     "un"       "un"       ""        "")
         , (2, forms "b"     "duo"      "duo"      "vi"      "du")
         , (3, forms "tr"    "tre"      "tres"     "tri"     "tre")
         , (4, forms "quadr" "quattuor" "quattuor" "quadra"  "quadri")
         , (5, forms "quint" "quin"     "quinqua"  "quinqua" "quin")
         , (6, forms "sext"  "sex"      "ses"      "sexa"    "ses")
         , (7, forms "sept"  "septen"   "septem"   "septua"  "septin")
         , (8, forms "oct"   "octo"     "octo"     "octo"    "octin")
         , (9, forms "non"   "novem"    "novem"    "nona"    "non")
         , (10, \c → case c of
                       CtxAdd _ (Lit 100) _              → "deci"
                       CtxMul _ _ (CtxAdd L (Lit 100) _) → "ginta"
                       CtxMul {}                         → "gint"
                       _                                 → "dec"
           )
         , (100, \c → case c of
                        CtxMul _ (Lit n) _
                            | n ∈ [2,3,6] → "cent"
                            | otherwise   → "gent"
                        _                 → "cent"
           )
         , (1000, const "millin")
         , (10000, const "myr")
         ]

forms ∷ s → s → s → s → s → Ctx Exp → s
forms t a1 a2 m1 m2 ctx =
    case ctx of
      CtxAdd _ (Lit 10)  _ → a1
      CtxAdd {}            → a2
      CtxMul _ (Lit 100) _ → m2
      CtxMul {}            → m1
      _                    → t

--------------------------------------------------------------------------------
-- Representations of scales
--------------------------------------------------------------------------------

scaleRepr ∷ (IsString s, Monoid s)
          ⇒ s -- ^Postfix for singular names.
          → s -- ^Postfix for plural names.
          → [(ℤ, Ctx Exp → s)]
          → ℤ → ℤ → Exp → Ctx Exp → Maybe s
scaleRepr s p syms _ _ e ctx = (⊕ pf) <$> render repr e
    where
      pf = case ctx of
             CtxMul _ (Lit 1) _ → s
             CtxMul {}          → p
             _                  → s
      repr = cardinalRepr { reprValue = \n → M.lookup n syms' }
      syms' = M.fromList syms ∪ symMap

pelletierRepr ∷ (IsString s, Monoid s)
              ⇒ s -- ^Postfix for singular offset 0 names.
              → s -- ^Postfix for singular offset 0 names.
              → s -- ^Postfix for plural offset 3 names.
              → s -- ^Postfix for plural offset 3 names.
              → [(ℤ, Ctx Exp → s)]
              → ℤ → ℤ → Exp → Ctx Exp → Maybe s
pelletierRepr s0 p0 s3 p3 syms
              b o e ctx | o ≡ 0 = scaleRepr s0 p0 syms b o e ctx
                        | o ≡ 3 = scaleRepr s3 p3 syms b o e ctx
                        | otherwise = Nothing

