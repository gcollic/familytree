{-# LANGUAGE OverloadedStrings #-}
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
  , tagFromText
  )
where

import           Data.Text                      ( Text )
import           Data.Maybe                     ( Maybe )
import           Data.Tree

type Entries = Forest Entry

data Entry = Entry {entryTag :: Tag,
        entryData :: Maybe Text}
  deriving (Eq, Show)

data Tag = INDI | NAME | GIVN | SURN | FAMC | FAMS | FAM | HUSB | WIFE | CHIL | OtherTag Text
  deriving (Eq, Show)

tagFromText :: Text -> Tag
tagFromText "INDI" = INDI
tagFromText "NAME" = NAME
tagFromText "GIVN" = GIVN
tagFromText "SURN" = SURN
tagFromText "FAMC" = FAMC
tagFromText "FAMS" = FAMS
tagFromText "FAM"  = FAM
tagFromText "HUSB" = HUSB
tagFromText "WIFE" = WIFE
tagFromText "CHIL" = CHIL
tagFromText t      = OtherTag t
