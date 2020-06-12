{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GedcomLibrarian
-- Copyright   :  (c) Guillaume Collic  2020
-- License     :  BSD-like
--
-- Stability   :  experimental
--
-- Looks for information in Gedcom data structure.
-----------------------------------------------------------------------------
module GedcomLibrarian
    ( findIndi
    )
where

import           Data.Text                      ( Text )
import           Data.Text.Internal.Search      ( indices )
import qualified Data.Text                     as T
import           GedcomData

findIndi :: Text -> [Entry] -> [Entry]
findIndi t =
    filter (\e -> entryTag e == "INDI" && any (isThisName t) (entryChildren e))

isThisName :: Text -> Entry -> Bool
isThisName text (Entry "NAME" (Just name) _) = text `isInside` name
isThisName _    _                            = False

isInside :: Text -> Text -> Bool
isInside a b = not $ null $ indices (T.toLower a) (T.toLower b)
