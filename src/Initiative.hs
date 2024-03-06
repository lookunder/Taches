{-# LANGUAGE DeriveGeneric, FlexibleInstances, OverloadedStrings  #-}

module Initiative (Initiative(..), Etat(..)) where

import Data.Aeson
import Data.List
import Control.Monad
import Control.Monad.Trans.Maybe
import CRUD
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Conversions
import Data.Time.Calendar
import Data.Time.Clock
--import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Data.UUID
import Data.UUID.V4
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

instance CRUD Initiative where
  ajouter fichier initiatives = creerInitiative >>= ajouterEtPersister fichier initiatives

  modifier fichier initiatives = do
    let table = convertirAvecEntête initiatives
        tableIndexee = Table.index <> table
    print tableIndexee >> sélectionnerContact >>= modifierEtPersister fichier initiatives

modifierEtPersister :: FilePath -> [Initiative] -> Maybe Integer -> IO [Initiative]
modifierEtPersister fichier elements mIndex = do
  case mIndex of
    Nothing -> return elements
    Just index -> do
      let (debut,f:fin) = genericSplitAt index elements
      mContact <- modifierInitiative f
      case mContact of
        Nothing -> return elements
        Just c -> do
              let elements' = debut <> (c:fin)
              encodeFile fichier elements'
              return elements'

ajouterEtPersister :: ToJSON a => FilePath -> [a] -> Maybe a -> IO [a]
ajouterEtPersister fichier elements mElement = do
  case mElement of
    Nothing -> return elements
    Just element -> do
      let elements' =  element:elements
      encodeFile fichier elements'
      return elements'

creerInitiative :: IO (Maybe Initiative)
creerInitiative = runMaybeT $ do
  uuid <- MaybeT (Just <$> nextRandom)
  nom <- MaybeT (TIO.putStr "Nom : " >> checkInput <$> TIO.getLine)
  creation <- MaybeT (Just <$> getCurrentTime)
  debut <- obtDate "Début" Nothing
  fin <- obtDate "Fin" Nothing
  return $ Initiative uuid nom creation debut fin Initial

modifierInitiative :: Initiative -> IO (Maybe Initiative)
modifierInitiative initiative = runMaybeT $ do
  nom   <- obtLigneAvecDéfaut "Nom" (nom initiative)
  debut <- obtDate "Début" (debut initiative)
  fin   <- obtDate "Fin" (fin initiative)
  return $ initiative { nom = nom, debut = debut, fin = fin}
