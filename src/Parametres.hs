module Parametres (Commande(..), obtOptions) where

import qualified Data.Text as T
import Options.Applicative

data Commande = Ajouter { texte :: T.Text }
              | Completer { position :: Int }
              | Effacer { position :: Int }
              | Lister { all :: Bool }
              | Annuler { position :: Int }


cmd :: Parser Commande
cmd = subparser
  ( command "ajouter" (info cmdAjouter ( progDesc "Ajouter une tâche." ))
 <> command "completer" (info cmdCompleter ( progDesc "Completer une tâche." ))
 <> command "effacer" (info cmdEffacer ( progDesc "Effacer les tâches." ))
 <> command "lister" (info cmdLister ( progDesc "Lister les tâches." ))
 <> command "annuler" (info cmdAnnuler ( progDesc "Annuler une tâche." ))
  )

cmdAjouter :: Parser Commande
cmdAjouter = Ajouter
     <$> argument str ( metavar "STRING" <> help "La tâche à ajouter." )

cmdCompleter :: Parser Commande
cmdCompleter = Completer
     <$> argument auto ( metavar "INTEGER" <> help "L'index de la tâche à completer." )

cmdAnnuler :: Parser Commande
cmdAnnuler = Annuler
     <$> argument auto ( metavar "INTEGER" <> help "L'index de la tâche à annuler." )

cmdEffacer :: Parser Commande
cmdEffacer = Effacer
     <$> argument auto ( metavar "INTEGER" <> help "L'index de la tâche à effacer." )

cmdLister :: Parser Commande
cmdLister = Lister
     <$> switch (short 'a')

opts = info (helper <*> cmd)
            (fullDesc <> progDesc "Gestion des tâches." <> header "tt - un programme pour gèrer les tâches.")

obtOptions = execParser opts