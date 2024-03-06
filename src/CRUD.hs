{-# LANGUAGE OverloadedStrings #-}

module CRUD (CRUD(..), eitherA2MaybeT, checkInput, sélectionnerContact, obtLigneAvecDéfaut, obtDate) where

import Data.Aeson
import Data.List
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Data.Time.Calendar
import Data.Time.Clock
import Data.Time.Format.ISO8601 (iso8601ParseM, iso8601Show)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Table

class (ToJSON a, Tabuler a) => CRUD a where
  ajouter  :: FilePath -> [a] -> IO [a]

  effacer  :: FilePath -> [a] -> IO [a]
  effacer fichier elements = do
    let table = convertirAvecEntête elements
        tableIndexee = Table.index <> table
    print tableIndexee >> sélectionnerContact >>= effacerEtPersister fichier elements

  lister   :: [a] -> IO [a]
  lister elements = do
    let table = convertirAvecEntête elements
        tableIndexee = Table.index <> table
    print tableIndexee >> return elements

  modifier :: FilePath -> [a] -> IO [a]

effacerEtPersister :: ToJSON a => FilePath -> [a] -> Maybe Integer -> IO [a]
effacerEtPersister fichier elements mIndex = do
  case mIndex of
    Nothing -> return elements
    Just index -> do
      let (debut,fin) = genericSplitAt index elements
          elements' = debut <> drop 1 fin
      encodeFile fichier elements'
      return elements'

sélectionnerContact :: IO (Maybe Integer)
sélectionnerContact = runMaybeT $ do
  numéro <- MaybeT (TIO.putStr "Numéro : " >> checkInput <$> TIO.getLine)
  eitherA2MaybeT . TR.decimal $ numéro

eitherA2MaybeT :: Either String (a, T.Text) -> MaybeT IO a
eitherA2MaybeT eData = do
  case eData of
    Right (donnee, reste) -> lift . return $ donnee
    Left msg  -> MaybeT (print msg >> return Nothing)

checkInput :: T.Text -> Maybe T.Text
checkInput ":q" = Nothing
checkInput i    = Just i

utiliseDéfaut :: T.Text -> T.Text -> T.Text
utiliseDéfaut défaut "" = défaut
utiliseDéfaut _      t  = t

obtLigneAvecDéfaut :: T.Text -> T.Text -> MaybeT IO T.Text
obtLigneAvecDéfaut question défaut = do
  MaybeT (TIO.putStr ( question <> "[" <> défaut <> "]" <>" : ") >> checkInput . utiliseDéfaut défaut <$> TIO.getLine)


formaterDate :: Maybe Day -> T.Text
formaterDate Nothing = ""
formaterDate (Just day) = T.pack(iso8601Show day)

obtDate :: T.Text -> Maybe Day -> MaybeT IO (Maybe Day)
obtDate question mValeur = do
  liftIO . TIO.putStr $ question <> "[" <> formaterDate mValeur <> "] : "
  texte <- liftIO TIO.getLine
  case texte of
    ":q" -> mzero           -- Nothing
    ":n" -> return mzero    -- Just Nothing
    ""   -> return mValeur  -- Just Maybe Day
    t    -> do
      let mDay = iso8601ParseM (T.unpack t)
      case mDay of
        Just d  -> return (Just d)
        Nothing -> obtDate question mValeur