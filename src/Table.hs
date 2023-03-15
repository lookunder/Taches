{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Table (Table(..), Tabuler(..), Aligner(..), (<>), show, index) where

import Data.List
import qualified Data.Text as T

class Tabuler a where
    convertirAvecEntête :: [a] -> Table

class Aligner a where
    aligner  :: a -> [T.Text]

data Table = 
    TableAvecEntete { données :: [[T.Text]]
                    , entete :: [T.Text]
                    } deriving (Eq)

instance Semigroup Table where
    (<>) :: Table -> Table -> Table
    (<>) (TableAvecEntete d1 e1) (TableAvecEntete d2 e2) =
        TableAvecEntete (zipWith (<>) d1 d2) (e1<>e2)

instance Show Table where
    show :: Table -> String
    show t = unlines $ map T.unpack (afficher t)

afficher :: Table -> [T.Text]
afficher table =
    let largeurs = largeurColonnes table
    in genererEntête (entete table) largeurs
       <> map (`deuxièmeLigne` largeurs) (données table)
       <> [dernièreLigne largeurs]

largeurColonnes :: Table -> [Int]
largeurColonnes (TableAvecEntete d e) = largeurColonnesImpl (e : d) (replicate (length e) 0)

largeurColonnesImpl :: [[T.Text]] -> [Int] -> [Int]
largeurColonnesImpl []     largeurs = largeurs
largeurColonnesImpl (t:ts) largeurs =
    let largeurCellulesLigne = map T.length t
        plusGrand = zipWith max largeurs largeurCellulesLigne
    in largeurColonnesImpl ts plusGrand

borduresHorizontales :: [Int] -> [T.Text]
borduresHorizontales = map (`T.replicate` "─")

pad :: (T.Text, Int) -> T.Text
pad (s,i) = s <> T.replicate (i - T.length s) (T.singleton ' ')

dernièreLigne :: [Int] -> T.Text
dernièreLigne colonnes = "└" <> T.intercalate "┴" lignes <> "┘"
                         where lignes = borduresHorizontales colonnes

premièreLigne :: [Int] -> T.Text
premièreLigne colonnes = "┌" <> T.intercalate "┬" lignes <> "┐"
                         where lignes = borduresHorizontales colonnes

deuxièmeLigne :: [T.Text] -> [Int] -> T.Text
deuxièmeLigne entete colonnes = "│" <> T.intercalate "│" cellules <> "│"
                                 where paires = zip entete colonnes
                                       cellules = map pad paires

troisièmeLigne :: [Int] -> T.Text
troisièmeLigne colonnes = "├" <> T.intercalate "┼" ligne <> "┤"
                          where ligne = borduresHorizontales colonnes

genererEntête :: [T.Text] -> [Int] -> [T.Text]
genererEntête toto colonnes = [ premièreLigne colonnes
                              , deuxièmeLigne toto colonnes
                            , troisièmeLigne colonnes]

-- FIXME: Éléminer quand base >= 4.15.0.0
singleton :: x -> [x]
singleton x = [x]

index :: Table
index = TableAvecEntete (map (singleton . T.pack . show) [0..]) [""]
