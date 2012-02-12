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

import "base" Data.Eq    ( Eq )
import "base" Data.Maybe ( Maybe(Nothing, Just) )
import "base" Text.Show  ( Show )
import "base-unicode-symbols" Data.Eq.Unicode ( (≡) )
import qualified "this" Text.Numeral.Grammar as G


-------------------------------------------------------------------------------
-- Inflection
-------------------------------------------------------------------------------

data Inflection = Inflection { iCase   ∷ Maybe Case
                             , iGender ∷ Maybe Gender
                             , iNumber ∷ Maybe Number
                             }

instance G.Inflection Inflection

defaultInflection ∷ Inflection
defaultInflection = Inflection Nothing Nothing Nothing


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

instance G.NoCase Inflection where
    noCase inf = inf { iCase = Nothing }
    hasNoCase (Inflection {iCase}) = iCase ≡ Nothing

instance G.Nominative Inflection where
    nominative inf = inf { iCase = Just Nominative }
    isNominative (Inflection {iCase}) = iCase ≡ Just Nominative

instance G.Accusative Inflection where
    accusative inf = inf { iCase = Just Accusative }
    isAccusative (Inflection {iCase}) = iCase ≡ Just Accusative

instance G.Dative Inflection where
    dative inf = inf { iCase = Just Dative }
    isDative (Inflection {iCase}) = iCase ≡ Just Dative

instance G.Ablative Inflection where
    ablative inf = inf { iCase = Just Ablative }
    isAblative (Inflection {iCase}) = iCase ≡ Just Ablative

instance G.Genitive Inflection where
    genitive inf = inf { iCase = Just Genitive }
    isGenitive (Inflection {iCase}) = iCase ≡ Just Genitive

instance G.Vocative Inflection where
    vocative inf = inf { iCase = Just Vocative }
    isVocative (Inflection {iCase}) = iCase ≡ Just Vocative

instance G.Locative Inflection where
    locative inf = inf { iCase = Just Locative }
    isLocative (Inflection {iCase}) = iCase ≡ Just Locative

instance G.Instrumental Inflection where
    instrumental inf = inf { iCase = Just Instrumental }
    isInstrumental (Inflection {iCase}) = iCase ≡ Just Instrumental


-------------------------------------------------------------------------------
-- Gender instances
-------------------------------------------------------------------------------

instance G.NoGender Inflection where
    noGender inf = inf { iGender = Nothing }
    hasNoGender (Inflection {iGender}) = iGender ≡ Nothing

instance G.Neuter Inflection where
    neuter inf = inf { iGender = Just Neuter }
    isNeuter (Inflection {iGender}) = iGender ≡ Just Neuter

instance G.Masculine Inflection where
    masculine inf = inf { iGender = Just Masculine }
    isMasculine (Inflection {iGender}) = iGender ≡ Just Masculine

instance G.Feminine Inflection where
    feminine inf = inf { iGender = Just Feminine }
    isFeminine (Inflection {iGender}) = iGender ≡ Just Feminine

instance G.Common Inflection where
    common inf = inf { iGender = Just Common }
    isCommon (Inflection {iGender}) = iGender ≡ Just Common


-------------------------------------------------------------------------------
-- Number instances
-------------------------------------------------------------------------------

instance G.NoNumber Inflection where
    noNumber inf = inf { iNumber = Nothing }
    hasNoNumber (Inflection {iNumber}) = iNumber ≡ Nothing

instance G.Singular Inflection where
    singular inf = inf { iNumber = Just Singular }
    isSingular (Inflection {iNumber}) = iNumber ≡ Just Singular

instance G.Dual Inflection where
    dual inf = inf { iNumber = Just Dual }
    isDual (Inflection {iNumber}) = iNumber ≡ Just Dual

instance G.Trial Inflection where
    trial inf = inf { iNumber = Just Trial }
    isTrial (Inflection {iNumber}) = iNumber ≡ Just Trial

instance G.Paucal Inflection where
    paucal inf = inf { iNumber = Just Paucal }
    isPaucal (Inflection {iNumber}) = iNumber ≡ Just Paucal

instance G.Plural Inflection where
    plural inf = inf { iNumber = Just Plural }
    isPlural (Inflection {iNumber}) = iNumber ≡ Just Plural
