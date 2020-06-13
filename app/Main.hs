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
  Show | FindIndividual Text | FindFamily Text Int | GenerateSvg Text Int
  deriving (Eq, Show)

main :: IO ()
main = getArgs >>= parseArgs >>= loadGedcom >>= act

parseArgs :: [String] -> IO (String, Action)
parseArgs args
  | ["-h"] <- args = usage >> exitSuccess
  | ["show", filepath] <- args = pure (filepath, Show)
  | ["find-individual", "--name", name, filepath] <- args = pure
    (filepath, FindIndividual $ T.pack name)
  | ["show-family", "--indi", indi, "--depth", depth, filepath] <- args = pure
    (filepath, FindFamily (T.pack indi) (read depth :: Int))
  | ["generate-svg", "--fam", fam, "--depth", depth, filepath] <- args = pure
    (filepath, GenerateSvg (T.pack fam) (read depth :: Int))
  | otherwise = usage >> exitWith (ExitFailure 1)
 where
  usage =
    Tio.putStrLn
      $ T.intercalate "\n"
      $ [ "Usage: familytree <command> <options...> <gedcom file>"
        , ""
        , "  WORK IN PROGRESS"
        , ""
        , "  This app aims to transforms genealogy data from GEDCOM file into"
        , "  beautiful radial genealogy tree in SVG, ready for laser engraving"
        , "  after minor adjustments (such as transform text to path with Inkscape)."
        , ""
        , "  It parses GEDCOM file without being too strict, in order to support"
        , "  most variants."
        , ""
        , "Commands :"
        , ""
        , "-h"
        , "    help/usage"
        , ""
        , "generate-svg --fam <fam> --depth <depth> <gedcom file>"
        , "    generate a radial genealogy tree in svg of the family"
        , "    <fam> - reference of the FAM entry (root family)"
        , "    <depth> - depth of ancestry to show"
        , ""
        , "show <gedcom file>"
        , "    print the whole parsed data, with indentation"
        , ""
        , "find-individual --name <name> <gedcom file>"
        , "    search and print gedcom INDI entries"
        , "    <name> - should match the NAME entry of the individual"
        , ""
        , "show-family --indi <indi> --depth <depth> <gedcom file>"
        , "    search and print the family tree of an individual"
        , "    <indi> - reference of the INDI entry (root individual)"
        , "    <depth> - depth of ancestry to show"
        ]

loadGedcom :: (String, Action) -> IO (Action, [Entry])
loadGedcom (filepath, a) = do
  content <- Tio.readFile (filepath :: FilePath)
  return (a, parseGedcom filepath content)

act :: (Action, [Entry]) -> IO ()
act (Show               , entries) = showEntries entries
act (FindIndividual name, entries) = showEntries $ findIndividual name entries
act (FindFamily indi depth, _) =
  Tio.putStrLn ("TODO FindFamily:" <> indi <> " / " <> (T.pack $ show depth))
act (GenerateSvg fam depth, _) =
  Tio.putStrLn ("TODO GenerateSvg:" <> fam <> " / " <> (T.pack $ show depth))

showEntries :: [Entry] -> IO ()
showEntries = Tio.putStrLn . (T.intercalate "\n") . gedcom2ascii

gedcom2ascii :: [Entry] -> [Text]
gedcom2ascii []                 = []
gedcom2ascii (Entry t c o : xs) = currentLine : (subItems ++ nextItems)
 where
  currentLine = T.intercalate "\t" [t, fromMaybe "" c]
  subItems    = map ("  " <>) $ gedcom2ascii o
  nextItems   = gedcom2ascii xs
