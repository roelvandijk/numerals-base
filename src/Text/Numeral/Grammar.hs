{-# LANGUAGE NoImplicitPrelude
           , UnicodeSyntax
           , PackageImports
  #-}

module Text.Numeral.Grammar
    ( -- * Grammatical categories
      -- ** Case
      Nominative   (nominative,   isNominative)
    , Accusative   (accusative,   isAccusative)
    , Dative       (dative,       isDative)
    , Ablative     (ablative,     isAblative)
    , Genitive     (genitive,     isGenitive)
    , Vocative     (vocative,     isVocative)
    , Locative     (locative,     isLocative)
    , Instrumental (instrumental, isInstrumental)
      -- ** Gender
    , Neuter       (neuter,       isNeuter)
    , Masculine    (masculine,    isMasculine)
    , Feminine     (feminine,     isFeminine)
    , Common       (common,       isCommon)
      -- ** Number
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
-- Case
--------------------------------------------------------------------------------

class Nominative α where
    nominative ∷ α → α
    isNominative ∷ α → Bool

class Accusative α where
    accusative ∷ α → α
    isAccusative ∷ α → Bool

class Dative α where
    dative ∷ α → α
    isDative ∷ α → Bool

class Ablative α where
    ablative ∷ α → α
    isAblative ∷ α → Bool

class Genitive α where
    genitive ∷ α → α
    isGenitive ∷ α → Bool

class Vocative α where
    vocative ∷ α → α
    isVocative ∷ α → Bool

class Locative α where
    locative ∷ α → α
    isLocative ∷ α → Bool

class Instrumental α where
    instrumental ∷ α → α
    isInstrumental ∷ α → Bool


--------------------------------------------------------------------------------
-- Gender
--------------------------------------------------------------------------------

class Neuter α where
    neuter ∷ α → α
    isNeuter ∷ α → Bool

class Masculine α where
    masculine ∷ α → α
    isMasculine ∷ α → Bool

class Feminine α where
    feminine ∷ α → α
    isFeminine ∷ α → Bool

class Common α where
    common ∷ α → α
    isCommon ∷ α → Bool


--------------------------------------------------------------------------------
-- Number
--------------------------------------------------------------------------------

class Singular α where
    singular ∷ α → α
    isSingular ∷ α → Bool

class Dual α where
    dual ∷ α → α
    isDual ∷ α → Bool

class Trial α where
    trial ∷ α → α
    isTrial ∷ α → Bool

class Paucal α where
    paucal ∷ α → α
    isPaucal ∷ α → Bool

class Plural α where
    plural ∷ α → α
    isPlural ∷ α → Bool

