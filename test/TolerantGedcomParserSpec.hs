{-# LANGUAGE OverloadedStrings #-}

module TolerantGedcomParserSpec
  ( spec
  )
where

import           Test.Hspec
import           Test.Hspec.Megaparsec
import           Text.Megaparsec
import qualified Data.Text                     as T
import           Data.Tree
import           TolerantGedcomParser
import           GedcomData

-- shortcut for clearer test
p :: Parsec e s a -> s -> Either (ParseErrorBundle s e) a
p f = parse f "TEST"

{- HLINT ignore main "Redundant do" -}
spec :: Spec
spec = do
  describe "GedCom line" $ do
    it "parses line with only indent, tag, and space"
      $             p (genericLine 0) "0 HEAD \r\n"
      `shouldParse` Node (Entry (OtherTag "HEAD") Nothing) []
    it "parses line with only indent, tag, without ending space"
      $             p (genericLine 0) "0 HEAD \r\n"
      `shouldParse` Node (Entry (OtherTag "HEAD") Nothing) []
    it "parses line with level, tag, and single word"
      $             p (genericLine 2) "2 GIVN Jeanne\r\n"
      `shouldParse` Node (Entry (OtherTag "GIVN") (Just "Jeanne")) []
    it "parses line with level, tag, and multiple words"
      $             p (genericLine 2) "2 DATE 14 MAY 1716\r\n"
      `shouldParse` Node (Entry (OtherTag "DATE") (Just "14 MAY 1716")) []
    it "parses content including tab"
      $             p (genericLine 3) "3 CONT Header1\tHeader2\tHeader3\r\n"
      `shouldParse` Node
                      (Entry (OtherTag "CONT")
                             (Just "Header1\tHeader2\tHeader3")
                      )
                      []
    it "parses tags with underscore "
      $             p (genericLine 1) "1 _FIL LEGITIMATE_CHILD\r\n"
      `shouldParse` Node (Entry (OtherTag "_FIL") (Just "LEGITIMATE_CHILD")) []
    it "parses tags with digit "
      $             p (genericLine 4) "4 ADR1 somewhere\r\n"
      `shouldParse` Node (Entry (OtherTag "ADR1") (Just "somewhere")) []
    it "parses root line with level, reference, and tag"
      $             p gedcom "0 @I1@ INDI\r\n"
      `shouldParse` [Node (Entry (OtherTag "INDI") (Just "@I1@")) []]
    it "parses root line, reference with underscore"
      $             p gedcom "0 @I_1@ INDI\r\n"
      `shouldParse` [Node (Entry (OtherTag "INDI") (Just "@I_1@")) []]
    it "parses root line, reference with lowercase"
      $             p gedcom "0 @i@ INDI\r\n"
      `shouldParse` [Node (Entry (OtherTag "INDI") (Just "@i@")) []]
    it "parses root line, reference with uppercase"
      $             p gedcom "0 @I@ INDI\r\n"
      `shouldParse` [Node (Entry (OtherTag "INDI") (Just "@I@")) []]
    it "parses root line, reference with digit"
      $             p gedcom "0 @I12@ INDI\r\n"
      `shouldParse` [Node (Entry (OtherTag "INDI") (Just "@I12@")) []]
  describe "GedCom file" $ do
    it "parses file without UTF-8 BOM"
      $             p gedcom "0 HEAD \r\n"
      `shouldParse` [Node (Entry (OtherTag "HEAD") Nothing) []]
    it "parses file with UTF-8 BOM"
      $             p gedcom (T.pack ('\65279' : "0 HEAD \r\n"))
      `shouldParse` [Node (Entry (OtherTag "HEAD") Nothing) []]
    it "parses items in a tree"
      $             p
                      gedcom
                      (T.intercalate
                        "\r\n"
                        [ "0 HEAD "
                        , "1 SOUR Heredis PC2017"
                        , "2 VERS 2017"
                        , "2 NAME Heredis PC"
                        , "2 CORP BSD Concept"
                        , "3 WEB www.heredis.com"
                        , "1 DATE 13 OCT 2016"
                        , ""
                        ]
                      )
      `shouldParse` [ Node
                        (Entry (OtherTag "HEAD") Nothing)
                        [ Node
                          (Entry (OtherTag "SOUR") (Just "Heredis PC2017"))
                          [ Node (Entry (OtherTag "VERS") (Just "2017")) []
                          , Node (Entry (OtherTag "NAME") (Just "Heredis PC"))
                                 []
                          , Node
                            (Entry (OtherTag "CORP") (Just "BSD Concept"))
                            [ Node
                                (Entry (OtherTag "WEB") (Just "www.heredis.com")
                                )
                                []
                            ]
                          ]
                        , Node (Entry (OtherTag "DATE") (Just "13 OCT 2016")) []
                        ]
                    ]
