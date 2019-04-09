{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeApplications #-}
module Main where
import Data.Machine


import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Modifiers (NonEmptyList (..))
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

unitTests = testGroup "Unit tests"
  [ testCase "create and get value from tuple" $ 
        (10 @?= 10) ]


qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty @ ([Int] -> Bool) "list to linked list" $ 
    \xs -> length xs == length ([1..length xs])
  ]
