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
  ( Entries
  , Entry(..)
  , Tag(..)
  )
where

import           Data.Text                      ( Text )
import           Data.Maybe                     ( Maybe )
import           Data.Tree

type Entries = Forest Entry

data Entry = Entry {entryTag :: Tag,
        entryData :: Maybe Text}
  deriving (Eq, Show)

data Tag = OtherTag Text
  deriving (Eq, Show)


