{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
--import Utils

import Table

testMAppend = TestCase (do let t1 = TableAvecEntete [["d11","d12"],["d21", "d22"]] ["e1", "e2"]
                               t2 = TableAvecEntete [["d13","d14"],["d23", "d24"]] ["e3", "e4"]
                               t3 = t1 <> t2
                               ref = TableAvecEntete [["d11","d12","d13","d14"],["d21", "d22","d23", "d24"]] ["e1", "e2", "e3", "e4"]
                           assertEqual "Les deux tables sont différentes." t3 ref
                     )

--pushTest :: Assertion
--pushTest = [NumLit 1] ^? push (NumLit 1)

--pushPopTest :: Assertion
--pushPopTest = [] ^? (push (NumLit 0) >> void pop)

tests = TestList [TestLabel "<>" testMAppend]

main :: IO ()
main = runTestTTAndExit tests