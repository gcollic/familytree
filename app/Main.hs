{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  )
where

import           TolerantGedcomParser
import           GedcomData
import           GedcomLibrarian
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as Tio
import           Data.Maybe
import           System.Exit
import           System.Environment

data Action =
  Print | FindIndi Text
  deriving (Eq, Show)

main :: IO ()
main = getArgs >>= parseArgs >>= loadGedcom >>= act

parseArgs :: [String] -> IO (String, Action)
parseArgs args
  | ["-h"] <- args = usage >> exitSuccess
  | ["print", filepath] <- args = pure (filepath, Print)
  | ["find_indi", "--name", name, filepath] <- args = pure (filepath, FindIndi $ T.pack name)
  | otherwise = usage >> exitWith (ExitFailure 1)
 where
  usage =
    Tio.putStrLn
      $ T.intercalate "\n"
      $ [ "Usage:"
        , "  familytree -h"
        , "  familytree print <gedcom file>"
        , "  familytree find_indi --name <name> <gedcom file>"
        ]

loadGedcom :: (String, Action) -> IO (Action, [Entry])
loadGedcom (filepath, a) = do
  content <- Tio.readFile (filepath :: FilePath)
  return (a, parseGedcom filepath content)

act :: (Action, [Entry]) -> IO ()
act (Print        , entries) = printEntries entries
act (FindIndi name, entries) = printEntries $ findIndi name entries

printEntries :: [Entry] -> IO ()
printEntries = Tio.putStrLn . (T.intercalate "\n") . gedcom2ascii

gedcom2ascii :: [Entry] -> [Text]
gedcom2ascii []                 = []
gedcom2ascii (Entry t c o : xs) = (currentLine : (subItems ++ nextItems))
 where
  currentLine = T.intercalate "\t" [t, (fromMaybe "" c)]
  subItems    = map ("  " <>) $ gedcom2ascii o
  nextItems   = gedcom2ascii xs
