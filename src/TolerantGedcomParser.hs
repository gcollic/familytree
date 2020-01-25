{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  TolerantGedcomParser
-- Copyright   :  (c) Guillaume Collic  2020
-- License     :  BSD-like
--
-- Stability   :  experimental
--
-- Gedcom file parser, tolerant to file diverging from standard
-- (such as Heredis 2017 variant).
-----------------------------------------------------------------------------
module TolerantGedcomParser
    ( parseGedcom
    , gedcom
    , leveledLine
    , Item(..)
    )
where

import           Data.Char                      ( intToDigit )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import           Debug.Trace                    ( trace )

type Parser = Parsec Void Text

data Item =
  Item {itemtag :: Text,
        itemcontent :: Maybe Text,
        subitems :: [Item]}
  deriving (Eq, Show)

parseGedcom :: String -> Text -> [Item]
parseGedcom f s = case parse gedcom f s of
    Left  e -> trace (errorBundlePretty e) $ error "Problem parsing the file"
    Right x -> x

gedcom :: Parser [Item]
gedcom = do
    rootItems <- many rootItem
    _         <- eof
    return rootItems

rootItem :: Parser Item
rootItem = try referenceLine <|> (leveledLine 0)

toMaybeText :: String -> Maybe Text
toMaybeText [] = Nothing
toMaybeText x  = pure $ T.pack x

referenceLine :: Parser Item
referenceLine = do
    _    <- string "0 @"
    ref  <- (many alphaNumChar)
    _    <- string "@ "
    tag  <- (string "FAM" <|> string "INDI")
    _    <- eol
    subs <- many $ leveledLine 1
    return $ Item tag (toMaybeText ("@" ++ ref ++ "@")) subs

leveledLine :: Int -> Parser Item
leveledLine i = do
    _       <- char $ intToDigit i
    _       <- char ' '
    tag     <- many (upperChar <|> char '_')
    _       <- char ' '
    content <- manyTill (printChar <|> tab) eol
    subs    <- many $ leveledLine (i + 1)
    return $ Item (T.pack tag) (toMaybeText content) subs
