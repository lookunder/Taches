{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text as T
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad

import Table

testMAppend = TestCase (do let t1 = TableAvecEntete [["d11","d12"],["d21", "d22"]] ["e1", "e2"]
                               t2 = TableAvecEntete [["d13","d14"],["d23", "d24"]] ["e3", "e4"]
                               t3 = t1 <> t2
                               ref = TableAvecEntete [["d11","d12","d13","d14"],["d21", "d22","d23", "d24"]] ["e1", "e2", "e3", "e4"]
                           assertEqual "Les deux tables sont différentes." t3 ref
                     )

data Test0 = Test0 { int0 :: Integer, int1 :: Integer } deriving (Show)

instance Aligner Test0 where
  aligner (Test0 a b) = [(T.pack . show) a, (T.pack . show) b]

instance Tabuler Test0 where
    convertirAvecEntête ts = let table = map aligner ts
                             in TableAvecEntete table ["Int 0", "Int 1"]

testAlignement = TestCase (  do let t = Test0 1 2
                                assertEqual "La fonction aligner ne fonctionne pas." ["1","2"] (aligner t)               
                          )

testTable = TestCase ( let t = Test0 1 2
                           table = convertirAvecEntête [t]
                       in assertEqual "La fonction convertirAvecEntête ne fonctionne pas." (TableAvecEntete [["1","2"]] ["Int 0", "Int 1"]) table              
                    )
--pushTest :: Assertion
--pushTest = [NumLit 1] ^? push (NumLit 1)

--pushPopTest :: Assertion
--pushPopTest = [] ^? (push (NumLit 0) >> void pop)
  
tests = TestList [TestLabel "<>" testMAppend, testAlignement, testTable]

main :: IO ()
main = runTestTTAndExit tests