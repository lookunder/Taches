module Main where

import Data.UUID()
import Data.UUID.V4(nextRandom)

main :: IO ()
main =  nextRandom >>= print