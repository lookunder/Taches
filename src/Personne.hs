{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Personne(Personne(..)) where

import Control.Monad.Trans.Maybe
import CRUD
import GHC.Generics
import Data.Aeson
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.UUID
import Data.UUID.V4
import Table
import qualified Telephone as Tel

data Personne =
    Personne { id :: UUID
             , nom :: T.Text
             , alias :: T.Text
             , altAccount :: T.Text
             , courriel :: T.Text
             , telephones :: [Tel.Telephone]
             } deriving (Generic, Show)

instance ToJSON Personne where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Personne

instance Tabuler Personne where
    convertirAvecEntête personnes = let table = map (\(Personne _ nom alias altAccount courriel _) -> [nom, alias, altAccount, courriel]) personnes
                                    in TableAvecEntete table ["Nom", "Alias", "Alt Account", "Courriel"]

instance CRUD Personne where
  ajouter fichier personnes = obtenirContact >>= ajouterEtPersister fichier personnes

  modifier fichier personnes = do
    let table = convertirAvecEntête personnes
        tableIndexee = Table.index <> table
    print tableIndexee >> sélectionnerContact >>= modifierEtPersister fichier personnes

modifierEtPersister :: FilePath -> [Personne] -> Maybe Integer -> IO [Personne]
modifierEtPersister fichier elements mIndex = do
  case mIndex of
    Nothing -> return elements
    Just index -> do
      let (debut,f:fin) = genericSplitAt index elements
      mContact <- modifierContact f
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

obtenirContact :: IO (Maybe Personne)
obtenirContact = runMaybeT $ do
  uuid <- MaybeT (Just <$> nextRandom)
  nom <- MaybeT (TIO.putStr "Nom : " >> checkInput <$> TIO.getLine)
  alias <- MaybeT (TIO.putStr "Alias : " >> checkInput <$> TIO.getLine)
  altAccount <- MaybeT (TIO.putStr "Alt Account : " >> checkInput <$> TIO.getLine)
  courriel <- MaybeT (TIO.putStr "Courriel : " >> checkInput <$> TIO.getLine)
  return $ Personne uuid nom alias altAccount courriel []

modifierContact :: Personne -> IO (Maybe Personne)
modifierContact personne = runMaybeT $ do
  uuid <- MaybeT (Just <$> nextRandom)
  nom <- obtLigneAvecDéfaut "Nom" (nom personne)
  alias <- obtLigneAvecDéfaut "Alias" (alias personne)
  altAccount <- obtLigneAvecDéfaut "Alt Account" (altAccount personne)
  courriel <- obtLigneAvecDéfaut "Courriel" (courriel personne)
  return $ Personne uuid nom alias altAccount courriel []