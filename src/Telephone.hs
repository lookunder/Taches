{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Telephone (Telephone(..), affiche) where

import GHC.Generics
import Data.Aeson
import qualified Data.Text as T

data TypeDeTelephone = Bureau | Cell deriving (Generic, Show)

instance ToJSON TypeDeTelephone where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON TypeDeTelephone

data Telephone =
    Telephone { typeDeTelephone :: TypeDeTelephone
              , numéro :: T.Text
              } deriving (Generic, Show)

instance ToJSON Telephone where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Telephone

affiche :: Telephone -> IO ()
affiche t = putStrLn $ (show . typeDeTelephone) t <> " : " <> (show . numéro) t