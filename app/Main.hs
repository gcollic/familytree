{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    )
where

import           TolerantGedcomParser
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as Tio
import           Data.Maybe
import           System.Exit
import           System.Environment

main :: IO ()
main = getArgs >>= parseArgs >>= printGedcom

parseArgs :: [String] -> IO (String, Text)
parseArgs args
    | ["-h"] <- args = usage >> exitSuccess
    | ['-' : _] <- args = usage >> exitWith (ExitFailure 1)
    | ["print", filepath] <- args = do
        content <- Tio.readFile (filepath :: FilePath)
        return (filepath, content)
    | otherwise = usage >> exitWith (ExitFailure 1)
  where
    usage =
        Tio.putStrLn
            $ T.intercalate "\n"
            $ ["Usage:", "  familytree -h", "  familytree print <gedcom file>"]

printGedcom :: (String, Text) -> IO ()
printGedcom (fileName, c) =
    putStrLn $ T.unpack $ T.intercalate "\n" $ gedcom2ascii $ parseGedcom
        fileName
        c


gedcom2ascii :: [Item] -> [Text]
gedcom2ascii []                = []
gedcom2ascii (Item t c o : xs) = (currentLine : (subItems ++ nextItems))
  where
    currentLine = T.intercalate "\t" [t, (fromMaybe "" c)]
    subItems    = map ("  " <>) $ gedcom2ascii o
    nextItems   = gedcom2ascii xs
