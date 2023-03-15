{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings  #-}

module Initiative (Initiative(..), Etat(..)) where

import Data.Aeson
import qualified Data.Text as T
import Data.Text.Conversions
import Data.Time.Calendar
import Data.Time.Clock
import Data.UUID
import GHC.Generics

import Table

data Etat = Initial | Actif | Complete | Annule deriving (Eq, Generic, Show)

instance ToJSON Etat where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Etat

data Initiative = 
    Initiative {
          id :: UUID
        , nom :: T.Text
        , creation  :: UTCTime
        , debut :: Maybe Day
        , fin :: Maybe Day
        , etat :: Etat
        } deriving (Generic, Show)

instance ToJSON Initiative where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Initiative

instance ToText (Maybe Day) where
  toText (Just d) = T.pack . show $ d
  toText Nothing  = ""

instance ToText Etat where
  toText = T.pack . show

instance ToText Integer where
  toText = T.pack . show

instance ToText UTCTime where
  toText = T.pack . show

instance Aligner Initiative where
  aligner (Initiative.Initiative _ nom creation debut fin etat) = 
    [convertText nom, convertText debut, convertText fin, convertText creation, convertText etat]

instance Tabuler Initiative where
    convertirAvecEntête initiatives = let table = map aligner initiatives
                                      in TableAvecEntete table ["Nom", "Début", "Fin", "Création", "État"]