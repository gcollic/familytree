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
    ( findIndividual
    )
where

import           Data.Text                      ( Text )
import           Data.Text.Internal.Search      ( indices )
import qualified Data.Text                     as T
import           GedcomData

findIndividual :: Text -> [Entry] -> [Entry]
findIndividual t =
    filter (\e -> entryTag e == "INDI" && any (isThatName t) (entryChildren e))

isThatName :: Text -> Entry -> Bool
isThatName text (Entry "NAME" (Just name) _) = text `isInside` name
isThatName _    _                            = False

isInside :: Text -> Text -> Bool
isInside a b = not $ null $ indices (T.toLower a) (T.toLower b)
