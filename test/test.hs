{-# LANGUAGE NoImplicitPrelude, UnicodeSyntax #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import System.IO ( IO )

-- from test-framework:
import Test.Framework ( Test, defaultMain )


--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

-- No tests yet :-(
tests ∷ [Test]
tests = []
