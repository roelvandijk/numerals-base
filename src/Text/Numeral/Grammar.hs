{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , PackageImports
  #-}

module Text.Numeral.Grammar
    ( -- * Inflection
      Inflection
      -- * Grammatical categories
      -- ** Case
    , NoCase       (noCase,       hasNoCase)
    , Nominative   (nominative,   isNominative)
    , Accusative   (accusative,   isAccusative)
    , Dative       (dative,       isDative)
    , Ablative     (ablative,     isAblative)
    , Genitive     (genitive,     isGenitive)
    , Vocative     (vocative,     isVocative)
    , Locative     (locative,     isLocative)
    , Instrumental (instrumental, isInstrumental)
      -- ** Gender
    , NoGender     (noGender,     hasNoGender)
    , Neuter       (neuter,       isNeuter)
    , Masculine    (masculine,    isMasculine)
    , Feminine     (feminine,     isFeminine)
    , Common       (common,       isCommon)
      -- ** Number
    , NoNumber     (noNumber,     hasNoNumber)
    , Singular     (singular,     isSingular)
    , Dual         (dual,         isDual)
    , Trial        (trial,        isTrial)
    , Paucal       (paucal,       isPaucal)
    , Plural       (plural,       isPlural)
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Bool ( Bool )


--------------------------------------------------------------------------------
-- Inflection
--------------------------------------------------------------------------------

-- | Base class for various kinds of inflections.
class Inflection α


--------------------------------------------------------------------------------
-- Case
--------------------------------------------------------------------------------

class Inflection α ⇒ NoCase α where
    noCase ∷ α → α
    hasNoCase ∷ α → Bool

class Inflection α ⇒ Nominative α where
    nominative ∷ α → α
    isNominative ∷ α → Bool

class Inflection α ⇒ Accusative α where
    accusative ∷ α → α
    isAccusative ∷ α → Bool

class Inflection α ⇒ Dative α where
    dative ∷ α → α
    isDative ∷ α → Bool

class Inflection α ⇒ Ablative α where
    ablative ∷ α → α
    isAblative ∷ α → Bool

class Inflection α ⇒ Genitive α where
    genitive ∷ α → α
    isGenitive ∷ α → Bool

class Inflection α ⇒ Vocative α where
    vocative ∷ α → α
    isVocative ∷ α → Bool

class Inflection α ⇒ Locative α where
    locative ∷ α → α
    isLocative ∷ α → Bool

class Inflection α ⇒ Instrumental α where
    instrumental ∷ α → α
    isInstrumental ∷ α → Bool


--------------------------------------------------------------------------------
-- Gender
--------------------------------------------------------------------------------

class Inflection α ⇒ NoGender α where
    noGender ∷ α → α
    hasNoGender ∷ α → Bool

class Inflection α ⇒ Neuter α where
    neuter ∷ α → α
    isNeuter ∷ α → Bool

class Inflection α ⇒ Masculine α where
    masculine ∷ α → α
    isMasculine ∷ α → Bool

class Inflection α ⇒ Feminine α where
    feminine ∷ α → α
    isFeminine ∷ α → Bool

class Inflection α ⇒ Common α where
    common ∷ α → α
    isCommon ∷ α → Bool


--------------------------------------------------------------------------------
-- Number
--------------------------------------------------------------------------------

class Inflection α ⇒ NoNumber α where
    noNumber ∷ α → α
    hasNoNumber ∷ α → Bool

class Inflection α ⇒ Singular α where
    singular ∷ α → α
    isSingular ∷ α → Bool

class Inflection α ⇒ Dual α where
    dual ∷ α → α
    isDual ∷ α → Bool

class Inflection α ⇒ Trial α where
    trial ∷ α → α
    isTrial ∷ α → Bool

class Inflection α ⇒ Paucal α where
    paucal ∷ α → α
    isPaucal ∷ α → Bool

class Inflection α ⇒ Plural α where
    plural ∷ α → α
    isPlural ∷ α → Bool

