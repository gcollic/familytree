{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as Tio
import System.Exit
import System.Environment

main :: IO ()
main = getArgs >>= parse >>= parseGedcom -- >>= print

parse :: [String] -> IO Text
parse args 
    | ["-h"] <- args = usage >> exitSuccess
    | (fs:_) <- args = Tio.readFile (fs :: FilePath)
    | otherwise  = usage >> exitWith (ExitFailure 1)
    where usage = Tio.putStrLn "Usage: familytree [-h] [file ..]"