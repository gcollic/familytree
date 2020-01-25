{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE RecordWildCards   #-}

module Lib
    ( parseGedcom,
    gedcom
    ) where

import Control.Applicative
import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import qualified Data.Text as T
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

parseGedcom :: Text -> IO () 
parseGedcom = parseTest gedcom

gedcom :: Parser Text
gedcom = string "data"
  <|> string "file"
  <|> string "ftp"
  <|> string "http"
  <|> string "https"
  <|> string "irc"
  <|> string "mailto"
