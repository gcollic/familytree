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
    , genericLine
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
    _         <- optional bom
    rootItems <- many rootItem
    _         <- eof
    return rootItems
    where bom = char '\65279'

rootItem :: Parser Item
rootItem = try referenceLine <|> (genericLine 0)

toMaybeText :: String -> Maybe Text
toMaybeText [] = Nothing
toMaybeText x  = pure $ T.pack x

referenceLine :: Parser Item
referenceLine = do
    _    <- string "0 @"
    ref  <- many (alphaNumChar <|> char '_')
    _    <- string "@ "
    tag  <- parseTag
    _    <- eol
    subs <- many $ genericLine 1
    return $ Item tag (toMaybeText ("@" ++ ref ++ "@")) subs

genericLine :: Int -> Parser Item
genericLine i = try (shortLine i) <|> (longLine i)

shortLine :: Int -> Parser Item
shortLine i = do
    _    <- char $ intToDigit i
    _    <- char ' '
    tag  <- parseTag
    _    <- eol
    subs <- many $ genericLine (i + 1)
    return $ Item tag Nothing subs

longLine :: Int -> Parser Item
longLine i = do
    _       <- char $ intToDigit i
    _       <- char ' '
    tag     <- parseTag
    _       <- char ' '
    content <- manyTill (printChar <|> tab) eol
    subs    <- many $ genericLine (i + 1)
    return $ Item tag (toMaybeText content) subs

parseTag :: Parser Text
parseTag = do
    t <- many (upperChar <|> char '_' <|> digitChar)
    return $ T.pack t
