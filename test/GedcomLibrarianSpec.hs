{-# LANGUAGE OverloadedStrings #-}

module GedcomLibrarianSpec
    ( spec
    )
where

import           Test.Hspec
import qualified Data.Text                     as T
import           GedcomLibrarian
import           TolerantGedcomParser
import           GedcomData

{- HLINT ignore main "Redundant do" -}
spec :: Spec
spec = describe "Find indi" $ do
    it "returns an empty collection if nothing is found" $ do
        findIndividual "bob" simpsons `shouldBe` []
    it "returns matching INDI entries (the text is the name)" $ do
        findIndividual "Maggie /Simpson/" simpsons
            `shouldBe` [ Entry "INDI"
                               (Just "@Maggie_Simpson@")
                               [Entry "NAME" (Just "Maggie /Simpson/") []]
                       ]
    it "returns matching INDI entries (the text is a part of the name)" $ do
        findIndividual "Simpson" simpsons
            `shouldBe` [ Entry "INDI"
                               (Just "@Abraham_Simpson@")
                               [Entry "NAME" (Just "Abraham /Simpson/") []]
                       , Entry "INDI"
                               (Just "@Bart_Simpson@")
                               [Entry "NAME" (Just "Bart /Simpson/") []]
                       , Entry "INDI"
                               (Just "@Homer_Simpson@")
                               [Entry "NAME" (Just "Homer /Simpson/") []]
                       , Entry "INDI"
                               (Just "@Lisa_Simpson@")
                               [Entry "NAME" (Just "Lisa /Simpson/") []]
                       , Entry "INDI"
                               (Just "@Maggie_Simpson@")
                               [Entry "NAME" (Just "Maggie /Simpson/") []]
                       , Entry "INDI"
                               (Just "@Marge_Simpson@")
                               [Entry "NAME" (Just "Marge /Simpson/") []]
                       , Entry "INDI"
                               (Just "@Mona_Simpson@")
                               [Entry "NAME" (Just "Mona /Simpson/") []]
                       ]
    it "ignores case" $ do
        findIndividual "Ma" simpsons
            `shouldBe` [ Entry "INDI"
                               (Just "@Maggie_Simpson@")
                               [Entry "NAME" (Just "Maggie /Simpson/") []]
                       , Entry "INDI"
                               (Just "@Marge_Simpson@")
                               [Entry "NAME" (Just "Marge /Simpson/") []]
                       , Entry "INDI"
                               (Just "@Selma_Bouvier@")
                               [Entry "NAME" (Just "Selma /Bouvier/") []]
                       ]

simpsons :: [Entry]
simpsons = parseGedcom
    (T.unpack "test")
    (T.intercalate
        "\r\n"
        [ "0 HEAD"
        , "1 SOUR https://github.com/jdfekete/geneaquilt/blob/master/src/data/Simpsons.ged"
        , "2 VERS fe0c45d"
        , "1 GEDC"
        , "2 VERS 5.5"
        , "0 @Abraham_Simpson@ INDI"
        , "1 NAME Abraham /Simpson/"
        , "0 @Bart_Simpson@ INDI"
        , "1 NAME Bart /Simpson/"
        , "0 @Clancy_Bouvier@ INDI"
        , "1 NAME Clancy /Bouvier/"
        , "0 @Homer_Simpson@ INDI"
        , "1 NAME Homer /Simpson/"
        , "0 @Jacqueline_Bouvier@ INDI"
        , "1 NAME Jacqueline /Bouvier/"
        , "0 @Lisa_Simpson@ INDI"
        , "1 NAME Lisa /Simpson/"
        , "0 @Maggie_Simpson@ INDI"
        , "1 NAME Maggie /Simpson/"
        , "0 @Marge_Simpson@ INDI"
        , "1 NAME Marge /Simpson/"
        , "0 @Mona_Simpson@ INDI"
        , "1 NAME Mona /Simpson/"
        , "0 @Patty_Bouvier@ INDI"
        , "1 NAME Patty /Bouvier/"
        , "0 @Selma_Bouvier@ INDI"
        , "1 NAME Selma /Bouvier/"
        , "0 @F0000@ FAM"
        , "1 HUSB @Homer_Simpson@"
        , "1 WIFE @Marge_Simpson@"
        , "1 CHIL @Bart_Simpson@"
        , "1 CHIL @Maggie_Simpson@"
        , "1 CHIL @Lisa_Simpson@"
        , "0 @F0002@ FAM"
        , "1 HUSB @Abraham_Simpson@"
        , "1 WIFE @Mona_Simpson@"
        , "1 CHIL @Homer_Simpson@"
        , "0 @F0003@ FAM"
        , "1 HUSB @Clancy_Bouvier@"
        , "1 WIFE @Jacqueline_Bouvier@"
        , "1 CHIL @Patty_Bouvier@"
        , "1 CHIL @Selma_Bouvier@"
        , "1 CHIL @Marge_Simpson@"
        , "0 TRLR"
        , ""
        ]
    )
