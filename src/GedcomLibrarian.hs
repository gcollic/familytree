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
import           Data.Tree
import           GedcomData

findIndividual :: Text -> Entries -> Entries
findIndividual t = filter (\n -> isIndiNode n && containsName n)
  where
    isIndiNode n = (entryTag . rootLabel) n == OtherTag "INDI"
    containsName n = any (isThatName t) (subForest n)

isThatName :: Text -> (Tree Entry) -> Bool
isThatName text (Node (Entry (OtherTag "NAME") (Just name)) _) =
    text `isInside` name
isThatName _ _ = False

isInside :: Text -> Text -> Bool
isInside a b = not $ null $ indices (T.toLower a) (T.toLower b)
