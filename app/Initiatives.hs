{-# LANGUAGE OverloadedStrings#-}

module Main where

import qualified Initiative

import System.Directory
import Control.Monad
import Data.Aeson

import Data.Maybe
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Clock
import System.FilePath ((</>))
import Options.Applicative
import Table
import Initiative (Initiative)

data Commande = Lister { all :: Bool }

cmd :: Parser Commande
cmd = subparser
  ( 
   command "lister" (info cmdLister ( progDesc "Lister les initiatives." ))
  )

cmdLister :: Parser Commande
cmdLister = Lister
     <$> switch (short 'a')

opts = info (helper <*> cmd)
            (fullDesc <> progDesc "Mes initiatives." <> header "mi - un programme pour gèrer les initiatives.")

obtOptions = execParser opts

lireFichierSiExiste :: FilePath -> IO (Either String [Initiative.Initiative])
lireFichierSiExiste fp = do
    existe <- doesFileExist fp
    if existe then eitherDecodeFileStrict' fp
              else return . Left $ "Impossible de trouver le fichier " <> fp <> "."

process :: Commande -> [Initiative.Initiative] -> IO [Initiative.Initiative]
process cmd personnes = do 
  let table = Table.convertirAvecEntête personnes
      tableIndexee = Table.index <> table
  case cmd of
    (Lister True)  -> print tableIndexee >> return personnes
    (Lister False) -> print tableIndexee >> return personnes

main :: IO ()
main = do
  options <- obtOptions
  home <- getHomeDirectory
  let fichier = home </> ".initiatives.json"
  eitherContacts <- lireFichierSiExiste fichier
  case eitherContacts of
    Left message   -> print message
    Right contacts -> void (process options contacts)