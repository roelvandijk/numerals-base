{-# LANGUAGE NoImplicitPrelude, PackageImports, UnicodeSyntax #-}

module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "base" System.IO ( IO )
import "test-framework" Test.Framework ( Test, defaultMain )


--------------------------------------------------------------------------------
-- Test suite
--------------------------------------------------------------------------------

main ∷ IO ()
main = defaultMain tests

-- No tests yet :-(
tests ∷ [Test]
tests = []
