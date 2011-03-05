{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , UnicodeSyntax
  #-}

module Text.Numeral.BigNum
  ( cardinal
  , rule
  , cardinalRepr
  , symMap
  , forms
  ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

-- from base:
import Data.Bool     ( otherwise )
import Data.Function ( const )
import Data.Maybe    ( Maybe(Just) )
import Data.Monoid   ( Monoid )
import Data.String   ( IsString )
import Prelude       ( Integral, Num )

-- from base-unicode-symbols:
import Data.List.Unicode ( (∈) )

-- from containers:
import qualified Data.Map as M ( Map, fromList, lookup )

-- from numerals:
import Text.Numeral


--------------------------------------------------------------------------------
-- Language of Big Numbers
--------------------------------------------------------------------------------

cardinal ∷ (Monoid s, IsString s, Integral α) ⇒ α → Maybe s
cardinal = mkCardinal rule cardinalRepr

rule ∷ (Integral α, Num β) ⇒ Rule α β
rule = findRule (1, atom)
                [ (  11, add  10 L  )
                , (  20, mul  10 L L)
                , ( 100, atom       )
                , ( 101, add 100 L  )
                , ( 200, mul 100 R L)
                , (1000, atom       )
                ]
                   1000

cardinalRepr ∷ (Monoid s, IsString s) ⇒ Repr s
cardinalRepr =
    defaultRepr { reprValue = \n → M.lookup n symMap
                , reprAdd   = \_ _ → Just ""
                , reprMul   = \_ _ → Just ""
                }

symMap ∷ (Integral α, IsString s) ⇒ M.Map α (SymbolContext → s)
symMap = M.fromList
         [ (1, forms "m"     "un"       "un"       "?"       "?")
         , (2, forms "b"     "duo"      "duo"      "vi"      "du")
         , (3, forms "tr"    "tre"      "tres"     "tri"     "tre")
         , (4, forms "quadr" "quattuor" "quattuor" "quadra"  "quadri")
         , (5, forms "quint" "quin"     "quinqua"  "quinqua" "quin")
         , (6, forms "sext"  "sex"      "ses"      "sexa"    "ses")
         , (7, forms "sept"  "septen"   "septem"   "septua"  "septin")
         , (8, forms "oct"   "octo"     "octo"     "octo"    "octin")
         , (9, forms "non"   "novem"    "novem"    "nona"    "non")
         , (10, \c → case c of
                       AddL (C 100) _ → "deci"
                       MulR _ (AddL (C 100) _) → "ginta"
                       MulR {}        → "gint"
                       _              → "dec"
           )
         , (100, \c → case c of
                        MulR (C n) _ | n ∈ [2,3,6] → "cent"
                                     | otherwise   → "gent"
                        _                          → "cent"
           )
         , (1000, const "millin")
         , (10000, const "myr")
         ]

forms ∷ s → s → s → s → s → SymbolContext → s
forms t a1 a2 m1 m2 ctx =
    case ctx of
      AddL (C 10)  _ → a1
      AddL {}        → a2
      MulL (C 100) _ → m2
      MulL {}        → m1
      _              → t
