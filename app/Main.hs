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
import           Data.Tree
import           Data.Maybe                     ( fromMaybe )
import           System.Exit
import           System.Environment
import           Rainbow

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

loadGedcom :: (String, Action) -> IO (Action, Entries)
loadGedcom (filepath, a) = do
  content <- Tio.readFile (filepath :: FilePath)
  return (a, parseGedcom filepath content)

act :: (Action, Entries) -> IO ()
act (Show               , entries) = showEntries entries
act (FindIndividual name, entries) = showEntries $ findIndividual name entries
act (FindFamily indi depth, _) =
  Tio.putStrLn ("TODO FindFamily:" <> indi <> " / " <> (T.pack $ show depth))
act (GenerateSvg fam depth, _) =
  Tio.putStrLn ("TODO GenerateSvg:" <> fam <> " / " <> (T.pack $ show depth))

showEntries :: Entries -> IO ()
showEntries = (mapM_ putChunk) . gedcom2ascii

gedcom2ascii :: Entries -> [Chunk Text]
gedcom2ascii [] = []
gedcom2ascii (Node (Entry t c) subs : next) =
  formatLine (tagText t) (tagColor t) (contentColor t)
    ++ [chunk "\n"]
    ++ subItems
    ++ nextItems
 where
  formatLine x c1 c2 =
    [chunk x & fore c1, chunk " ", (chunk $ fromMaybe "" c) & fore c2]
  subItems  = map (chunk "  " <>) $ gedcom2ascii subs
  nextItems = gedcom2ascii next
  tagColor (OtherTag x) = color256 240
  tagColor INDI         = color256 63
  tagColor NAME         = color256 69
  tagColor GIVN         = color256 75
  tagColor SURN         = color256 75
  tagColor FAMS         = color256 69
  tagColor FAMC         = color256 69
  tagColor FAM          = color256 202
  tagColor HUSB         = color256 208
  tagColor WIFE         = color256 208
  tagColor CHIL         = color256 208
  tagColor x            = red
  contentColor (OtherTag x) = color256 240
  contentColor FAMS         = color256 202
  contentColor FAMC         = color256 202
  contentColor HUSB         = color256 63
  contentColor WIFE         = color256 63
  contentColor CHIL         = color256 63
  contentColor x            = white
  tagText (OtherTag x) = x
  tagText x            = (T.pack $ show x)
