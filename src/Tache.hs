{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

module Tache ( Tache(..), lister, ajouter, completer, effacer, annuler, afficherLigne, Etat(..)) where

import GHC.Generics
import Data.Time.Clock
import Data.Aeson
import qualified Data.Text as T

data Etat = Actif | Complete | Annule deriving (Eq, Generic, Show)

instance ToJSON Etat where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Etat

data Tache = 
    Tache { nom :: T.Text
          , creation  :: UTCTime
          , etat :: Etat
          } deriving (Generic, Show)

instance ToJSON Tache where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Tache

completer :: Int -> [Tache] -> [Tache]
completer i = changerEtat i Complete

annuler :: Int ->[Tache] ->  [Tache]
annuler i = changerEtat i Annule

changerEtat :: Int -> Etat -> [Tache] ->  [Tache]
changerEtat i etat ts | i < length ts = let (debut, elem:fin) = splitAt i ts
                                        in debut <> ( elem { etat = etat } : fin )
                      | otherwise     = ts

actualiser :: Int -> T.Text -> [Tache] -> [Tache]
actualiser i s ts = undefined

ajouter :: [Tache] -> T.Text -> IO [Tache]
ajouter ts s = do
    currentTime <- getCurrentTime
    let tache = Tache s currentTime Actif
    return $ tache : ts

lister ::  Bool -> [(Int,Tache)] ->[(Int,Tache)]
lister True ts  = ts
lister False ts = filter (\(i,t) -> Actif == etat t) ts

effacer :: Int -> [Tache] -> [Tache]
effacer i ts | i < length ts = let (debut, elem:fin) = splitAt i ts
                               in debut <> fin
             | otherwise     = ts

afficherLigne :: [(Int,Tache)] -> IO ()
afficherLigne ((i,t):xs) = putStrLn (show i <> " " <> (T.unpack . afficher) t) >> afficherLigne xs
afficherLigne []         = return ()

afficher :: Tache -> T.Text
afficher (Tache nom _ Complete) = "\ESC[31mX\ESC[0m" <> " " <> nom
afficher (Tache nom _ Actif)    = "-" <> " " <> nom
afficher (Tache nom _ Annule)   = "\ESC[93mA\ESC[0m" <> " " <> nom