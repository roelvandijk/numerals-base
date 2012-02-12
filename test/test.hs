{-# LANGUAGE NoImplicitPrelude, PackageImports, UnicodeSyntax #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" Data.Bool ( Bool )
import "base" Data.Function ( ($) )
import "base" Prelude ( (+), (-) )
import "base" System.IO ( IO )
import "base-unicode-symbols" Data.Eq.Unicode ( (≡) )
import "base-unicode-symbols" Prelude.Unicode ( ℤ, (⋅) )
import "HUnit" Test.HUnit ( Assertion, assertBool )
import "QuickCheck" Test.QuickCheck.Modifiers
    ( Positive(Positive), NonNegative(NonNegative) )
import "test-framework" Test.Framework ( defaultMain, Test, testGroup )
import "test-framework-hunit" Test.Framework.Providers.HUnit ( testCase )
import "test-framework-quickcheck2" Test.Framework.Providers.QuickCheck2 ( testProperty )
import           "numerals-base" Text.Numeral.Grammar
import qualified "numerals-base" Text.Numeral.Grammar.Reified as GR
import           "numerals-base" Text.Numeral.Misc ( dec, intLog )

--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

tests ∷ [Test]
tests = [ testGroup "Grammar"
          [ testGroup "Case"
            [ testCase "noCase"       $ testInf noCase       hasNoCase
            , testCase "nominative"   $ testInf nominative   isNominative
            , testCase "accusative"   $ testInf accusative   isAccusative
            , testCase "dative"       $ testInf dative       isDative
            , testCase "ablative"     $ testInf ablative     isAblative
            , testCase "genitive"     $ testInf genitive     isGenitive
            , testCase "vocative"     $ testInf vocative     isVocative
            , testCase "instrumental" $ testInf instrumental isInstrumental
            ]
          , testGroup "Gender"
            [ testCase "noGender"  $ testInf noGender  hasNoGender
            , testCase "neuter"    $ testInf neuter    isNeuter
            , testCase "masculine" $ testInf masculine isMasculine
            , testCase "feminine"  $ testInf feminine  isFeminine
            , testCase "common"    $ testInf common    isCommon
            ]
          , testGroup "Number"
            [ testCase "noNumber" $ testInf noNumber hasNoNumber
            , testCase "singular" $ testInf singular isSingular
            , testCase "dual"     $ testInf dual     isDual
            , testCase "trial"    $ testInf trial    isTrial
            , testCase "paucal"   $ testInf paucal   isPaucal
            , testCase "plural"   $ testInf plural   isPlural
            ]
          ]
        , testGroup "Misc"
          [ testGroup "intLog"
            [ testProperty "power of 10" intLog_pow10
            , testProperty "power of 10 minus 1" intLog_pow10m1
            , testProperty "multiply" intLog_mul
            ]
          ]
        ]

testInf ∷ (GR.Inflection → GR.Inflection) → (GR.Inflection → Bool) → Assertion
testInf set test = assertBool "False" $ test $ set GR.defaultInflection

intLog_pow10 ∷ Positive ℤ → Bool
intLog_pow10 (Positive x) = x ≡ intLog (dec x)

intLog_pow10m1 ∷ NonNegative ℤ → Bool
intLog_pow10m1 (NonNegative x) = x - 1 ≡ intLog (dec x) - 1

intLog_mul ∷ Positive ℤ → Positive ℤ → Bool
intLog_mul (Positive x) (Positive y) = intLog (dec x ⋅ dec y) ≡ intLog (dec x) + intLog (dec y)
