{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GedcomData
-- Copyright   :  (c) Guillaume Collic  2020
-- License     :  BSD-like
--
-- Stability   :  experimental
--
-- Simple Gedcom data structure.
-----------------------------------------------------------------------------
module GedcomData
  ( Entry(..)
  )
where

import           Data.Text                      ( Text )
import           Data.Maybe                     ( Maybe )

data Entry =
  Entry {entryTag :: Text,
        entryData :: Maybe Text,
        entryChildren :: [Entry]}
  deriving (Eq, Show)
