{-# LANGUAGE NamedFieldPuns
           , NoImplicitPrelude
           , UnicodeSyntax
           , PackageImports
  #-}

module Text.Numeral.Grammar.Reified
    ( -- * Inflection
      Inflection(..)
    , defaultInflection
      -- * Grammatical categories
    , Case(..)
    , Gender(..)
    , Number(..)
    ) where


-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

import "base" Data.Eq   ( Eq )
import "base" Text.Show ( Show )
import "base-unicode-symbols" Data.Eq.Unicode ( (≡) )
import qualified "this" Text.Numeral.Grammar as G


-------------------------------------------------------------------------------
-- Inflection
-------------------------------------------------------------------------------

data Inflection = Inflection { iCase   ∷ Case
                             , iGender ∷ Gender
                             , iNumber ∷ Number
                             }

defaultInflection ∷ Inflection
defaultInflection = Inflection Nominative Neuter Singular


-------------------------------------------------------------------------------
-- Grammatical categories
-------------------------------------------------------------------------------

data Case = Nominative
          | Accusative
          | Dative
          | Ablative
          | Genitive
          | Vocative
          | Locative
          | Instrumental
            deriving (Eq, Show)

data Gender = Neuter
            | Masculine
            | Feminine
            | Common
              deriving (Eq, Show)

data Number = Singular
            | Dual
            | Trial
            | Paucal
            | Plural
              deriving (Eq, Show)


-------------------------------------------------------------------------------
-- Case instances
-------------------------------------------------------------------------------

instance G.Nominative Inflection where
    nominative inf = inf { iCase = Nominative }
    isNominative (Inflection {iCase}) = iCase ≡ Nominative

instance G.Accusative Inflection where
    accusative inf = inf { iCase = Accusative }
    isAccusative (Inflection {iCase}) = iCase ≡ Accusative

instance G.Dative Inflection where
    dative inf = inf { iCase = Dative }
    isDative (Inflection {iCase}) = iCase ≡ Dative

instance G.Ablative Inflection where
    ablative inf = inf { iCase = Ablative }
    isAblative (Inflection {iCase}) = iCase ≡ Ablative

instance G.Genitive Inflection where
    genitive inf = inf { iCase = Genitive }
    isGenitive (Inflection {iCase}) = iCase ≡ Genitive

instance G.Vocative Inflection where
    vocative inf = inf { iCase = Vocative }
    isVocative (Inflection {iCase}) = iCase ≡ Vocative

instance G.Locative Inflection where
    locative inf = inf { iCase = Locative }
    isLocative (Inflection {iCase}) = iCase ≡ Locative

instance G.Instrumental Inflection where
    instrumental inf = inf { iCase = Instrumental }
    isInstrumental (Inflection {iCase}) = iCase ≡ Instrumental


-------------------------------------------------------------------------------
-- Gender instances
-------------------------------------------------------------------------------

instance G.Neuter Inflection where
    neuter inf = inf { iGender = Neuter }
    isNeuter (Inflection {iGender}) = iGender ≡ Neuter

instance G.Masculine Inflection where
    masculine inf = inf { iGender = Masculine }
    isMasculine (Inflection {iGender}) = iGender ≡ Masculine

instance G.Feminine Inflection where
    feminine inf = inf { iGender = Feminine }
    isFeminine (Inflection {iGender}) = iGender ≡ Feminine

instance G.Common Inflection where
    common inf = inf { iGender = Common }
    isCommon (Inflection {iGender}) = iGender ≡ Common


-------------------------------------------------------------------------------
-- Number instances
-------------------------------------------------------------------------------

instance G.Singular Inflection where
    singular inf = inf { iNumber = Singular }
    isSingular (Inflection {iNumber}) = iNumber ≡ Singular

instance G.Dual Inflection where
    dual inf = inf { iNumber = Dual }
    isDual (Inflection {iNumber}) = iNumber ≡ Dual

instance G.Trial Inflection where
    trial inf = inf { iNumber = Trial }
    isTrial (Inflection {iNumber}) = iNumber ≡ Trial

instance G.Paucal Inflection where
    paucal inf = inf { iNumber = Paucal }
    isPaucal (Inflection {iNumber}) = iNumber ≡ Paucal

instance G.Plural Inflection where
    plural inf = inf { iNumber = Plural }
    isPlural (Inflection {iNumber}) = iNumber ≡ Plural
