module Main where

import qualified Tache
import qualified Parametres

import Data.Time.Clock
import System.Directory
import Data.Monoid ((<>))
import Data.Aeson
import Data.Maybe
import System.FilePath

lireFichierSiExiste :: FilePath -> IO [Tache.Tache]
lireFichierSiExiste fp = do
    existe <- doesFileExist fp
    if existe then
      do
        taches <- decodeFileStrict fp
        return $ fromMaybe [] taches
    else return []

numéroter :: [a] -> [(Int,a)]
numéroter = zip [0..]

main :: IO ()
main = do
  options <- Parametres.obtOptions
  home <- getHomeDirectory
  let fichier = home </> ".taches.json"
  taches <- lireFichierSiExiste fichier
  tachesActualisees <- case options of
    (Parametres.Ajouter msg)  -> Tache.ajouter taches msg
    (Parametres.Completer i)  -> return $ Tache.completer i taches
    (Parametres.Effacer i)    -> return $ Tache.effacer i taches
    (Parametres.Annuler i)    -> return $ Tache.annuler i taches
    (Parametres.Lister True)  -> (Tache.afficherLigne . Tache.lister True  . numéroter $ taches) >> return taches
    (Parametres.Lister False) -> (Tache.afficherLigne . Tache.lister False . numéroter $ taches) >> return taches
  encodeFile fichier tachesActualisees