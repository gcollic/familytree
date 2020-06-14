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
-- Gedcom file parser, tolerant to variations.
-- Strict reference : http://homepages.rootsweb.com/~pmcbride/gedcom/55gctoc.htm
-----------------------------------------------------------------------------
module TolerantGedcomParser
    ( parseGedcom
    , gedcom
    , genericLine
    )
where

import           Data.Char                      ( intToDigit )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Tree
import           Data.Void
import           Text.Megaparsec         hiding ( State )
import           Text.Megaparsec.Char
import           Debug.Trace                    ( trace )
import           GedcomData

type Parser = Parsec Void Text

parseGedcom :: String -> Text -> Entries
parseGedcom f s = case parse gedcom f s of
    Left  e -> trace (errorBundlePretty e) $ error "Problem parsing the file"
    Right x -> x

gedcom :: Parser Entries
gedcom = do
    _           <- optional bom
    rootEntries <- many rootEntry
    _           <- eof
    return rootEntries
    where bom = char '\65279'

rootEntry :: Parser (Tree Entry)
rootEntry = try referenceLine <|> (genericLine 0)

referenceLine :: Parser (Tree Entry)
referenceLine = do
    _    <- char '0'
    _    <- char ' '
    ref  <- reference
    _    <- char ' '
    t    <- tag
    _    <- eol
    subs <- many $ genericLine 1
    return $ Node (Entry t (Just ref)) subs

reference :: Parser Text
reference = do
    _   <- char '@'
    ref <- many (alphaNumChar <|> char '_')
    _   <- char '@'
    return $ T.pack $ "@" ++ ref ++ "@"

genericLine :: Int -> Parser (Tree Entry)
genericLine i = do
    _    <- char $ intToDigit i
    _    <- char ' '
    t    <- tag
    val  <- optional content
    _    <- eol
    subs <- many $ genericLine (i + 1)
    return $ Node (Entry t (nothingIfEmpty $ T.strip . T.pack <$> val)) subs
  where
    nothingIfEmpty (Just "") = Nothing
    nothingIfEmpty x         = x

content :: Parser String
content = do
    _ <- char ' '
    many (printChar <|> tab)

tag :: Parser Tag
tag = tagFromText <$> T.pack <$> many (upperChar <|> char '_' <|> digitChar)
