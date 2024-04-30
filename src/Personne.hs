{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Personne(Personne(..)) where

import Control.Monad.Trans.Maybe
import CRUD
import GHC.Generics
import Data.Aeson
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Conversions
--import qualified Data.Text.Read as TR
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

instance Aligner Personne where
  aligner (Personne.Personne _ nom alias altAccount courriel _) = 
    [convertText nom, convertText alias, convertText altAccount, convertText courriel]

instance Tabuler Personne where
    convertirAvecEntête personnes = let table = map aligner personnes
                                    in TableAvecEntete table ["Nom", "Alias", "Alt Account", "Courriel"]

instance CRUD Personne where

  ajouterIO = runMaybeT $ do
    uuid <- MaybeT (Just <$> nextRandom)
    nom <- MaybeT (TIO.putStr "Nom : " >> checkInput <$> TIO.getLine)
    alias <- MaybeT (TIO.putStr "Alias : " >> checkInput <$> TIO.getLine)
    altAccount <- MaybeT (TIO.putStr "Alt Account : " >> checkInput <$> TIO.getLine)
    courriel <- MaybeT (TIO.putStr "Courriel : " >> checkInput <$> TIO.getLine)
    return $ Personne uuid nom alias altAccount courriel []

  modifierIO personne = runMaybeT $ do
    uuid <- MaybeT (Just <$> nextRandom)
    nom <- obtLigneAvecDéfaut "Nom" (nom personne)
    alias <- obtLigneAvecDéfaut "Alias" (alias personne)
    altAccount <- obtLigneAvecDéfaut "Alt Account" (altAccount personne)
    courriel <- obtLigneAvecDéfaut "Courriel" (courriel personne)
    return $ Personne uuid nom alias altAccount courriel []
