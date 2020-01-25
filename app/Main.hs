{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    )
where

import           TolerantGedcomParser
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as Tio
import           Data.List
import           Data.Maybe
import           System.Exit
import           System.Environment

main :: IO ()
main = getArgs >>= parseArgs >>= printGedcom

parseArgs :: [String] -> IO (String, Text)
parseArgs args
    | ["-h"] <- args = usage >> exitSuccess
    | ['-' : _] <- args = usage >> exitWith (ExitFailure 1)
    | [fp] <- args = do
        c <- Tio.readFile (fp :: FilePath)
        return (fp, c)
    | otherwise = usage >> exitWith (ExitFailure 1)
    where usage = Tio.putStrLn "Usage: familytree [-h] [file]"

printGedcom :: (String, Text) -> IO ()
printGedcom (fileName, c) =
    putStrLn $ intercalate "\n" $ gedcom2ascii $ parseGedcom fileName c


gedcom2ascii :: [Item] -> [String]
gedcom2ascii []                = []
gedcom2ascii (Item t c o : xs) = (currentLine : nextStrings)
  where
    currentLine = T.unpack t ++ "\t" ++ T.unpack (fromMaybe "" c)
    nextStrings = (subItems ++ nextItems)
    subItems    = map ("  " <>) $ gedcom2ascii o
    nextItems   = gedcom2ascii xs
