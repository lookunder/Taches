{-# LANGUAGE OverloadedStrings#-}

module Main where

import qualified Initiative

import Table
import System.Directory
import Control.Monad
import CRUD(modifier, lister, ajouter, effacer)
import Data.Aeson
import Data.Maybe
import System.FilePath ((</>))
import Options.Applicative
import System.IO ( stdout, hSetBuffering, BufferMode(NoBuffering) )

data Commande = Lister { all :: Bool }
              | Ajouter
              | Effacer
              | Modifier

cmd :: Parser Commande
cmd = subparser
  ( 
   command "lister" (info cmdLister ( progDesc "Lister les initiatives." ))
   <> command "ajouter" (info cmdAjouter ( progDesc "Ajoutter une initiative." ))
   <> command "effacer" (info cmdEffacer ( progDesc "Effacer une initiative." ))
   <> command "modifier" (info cmdModifier ( progDesc "Modifier une initiative." ))
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
            (fullDesc <> progDesc "Mes initiatives." <> header "mi - un programme pour gèrer les initiatives.")

obtOptions = execParser opts

lireFichierSiExiste :: FilePath -> IO (Either String [Initiative.Initiative])
lireFichierSiExiste fp = do
    existe <- doesFileExist fp
    if existe then eitherDecodeFileStrict' fp
              else return . Left $ "Impossible de trouver le fichier " <> fp <> "."

process :: Commande -> FilePath -> [Initiative.Initiative] -> IO [Initiative.Initiative]
process cmd fichier personnes = do 
  let table = Table.convertirAvecEntête personnes
      tableIndexee = Table.index <> table
  case cmd of
    (Lister True)  -> print tableIndexee >> return personnes
    (Lister False) -> print tableIndexee >> return personnes
    Ajouter        -> ajouter fichier personnes
    Effacer        -> effacer fichier personnes
    Modifier       -> modifier fichier personnes

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  options <- obtOptions
  home <- getHomeDirectory
  let fichier = home </> ".initiatives.json"
  eitherContacts <- lireFichierSiExiste fichier
  case eitherContacts of
    Left message   -> print message
    Right contacts -> void (process options fichier contacts)
