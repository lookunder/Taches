{-# LANGUAGE OverloadedStrings #-}

module Main where

import Table
import Control.Monad ( void )
import CRUD(modifier, lister, ajouter, effacer)
import Data.Aeson
import Options.Applicative
import Personne
import System.Directory ( doesFileExist, getHomeDirectory )
import System.FilePath ( (</>) )
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )

data Commande = Lister { all :: Bool }
              | Ajouter
              | Effacer
              | Modifier

cmd :: Parser Commande
cmd = subparser
  ( 
      command "lister" (info cmdLister ( progDesc "Lister les contacts." ))
   <> command "ajouter" (info cmdAjouter ( progDesc "Ajouter un contact." ))
   <> command "effacer" (info cmdEffacer ( progDesc "Effacer un contact." ))
   <> command "modifier" (info cmdModifier ( progDesc "Modifier un contact." ))
  )

cmdLister :: Parser Commande
cmdLister = Lister
     <$> switch (short 'a')

cmdAjouter :: Parser Commande
cmdAjouter = pure Ajouter

cmdEffacer :: Parser Commande
cmdEffacer = pure Effacer

cmdModifier :: Parser Commande
cmdModifier = pure Modifier

opts = info (helper <*> cmd)
            (fullDesc <> progDesc "Gestion des tâches." <> header "pp - un programme pour gèrer les contacts.")

obtOptions = execParser opts

lireFichierSiExiste :: FilePath -> IO (Either String [Personne])
lireFichierSiExiste fp = do
    existe <- doesFileExist fp
    if existe then eitherDecodeFileStrict' fp
              else return . Left $ "Impossible de trouver le fichier " <> fp <> "."

process :: Commande -> FilePath -> [Personne] -> IO [Personne]
process cmd fichier personnes = do 
  let table = convertirAvecEntête personnes
      tableIndexee = Table.index <> table
  --Faire le mapping
  case cmd of
    (Lister True)  -> lister personnes
    (Lister False) -> lister personnes
    Ajouter        -> ajouter fichier personnes
    Effacer        -> effacer fichier personnes
    Modifier       -> modifier fichier personnes

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  options <- obtOptions
  home <- getHomeDirectory
  let fichier = home </> ".contacts.json"
  eitherContacts <- lireFichierSiExiste fichier
  case eitherContacts of
    Left message   -> print message
    Right contacts -> void (process options fichier contacts)