module Text.Parsec.Indent.Tests
    ( tests
    ) where

import           Control.Applicative ((*>), (<*))
import qualified Test.Tasty          as Tasty
import qualified Test.Tasty.HUnit    as HUnit
import qualified Text.Parsec         as Parsec
import qualified Text.Parsec.Indent  as Indent
import           Prelude

tests :: Tasty.TestTree
tests = Tasty.testGroup "Text.Parsec.Indent.Tests"
    [ HUnit.testCase "01" $ parseTaxonomyTest
        "k       \n\
        \ a1     \n\
        \ a2     \n\
        \  b1    \n\
        \ a3     \n\
        \  haha  \n\
        \  it    \n\
        \  works \n\
        \" $ Just $
        Taxonomy "k"
            [ Taxonomy "a1" []
            , Taxonomy "a2"
                [ Taxonomy "b1" []
                ]
            , Taxonomy "a3"
                [ Taxonomy "haha"  []
                , Taxonomy "it"    []
                , Taxonomy "works" []
                ]
            ]

    , HUnit.testCase "02" $ parseTaxonomyTest
        " k \n\
        \a  \n\
        \"
        Nothing
    ]

type Term = String

data Taxonomy = Taxonomy Term [Taxonomy] deriving (Eq, Show)

pTerm :: Indent.IndentParser String () String
pTerm = Parsec.many1 Parsec.alphaNum <* Parsec.spaces

pTaxonomy :: Indent.IndentParser String () Taxonomy
pTaxonomy = Indent.withPos $ do
    term <- pTerm
    subs <- Parsec.many $ Indent.indented *> pTaxonomy
    return $ Taxonomy term subs

parseTaxonomyTest :: String -> Maybe Taxonomy -> HUnit.Assertion
parseTaxonomyTest src mbResult = case errOrTax of
    Left err -> case mbResult of
        Just _  -> HUnit.assertFailure (show err)
        Nothing -> return ()
    Right tax -> case mbResult of
        Just expected -> HUnit.assertEqual "parseTaxonomyTest" expected tax
        Nothing       -> HUnit.assertFailure $
            "Expected parse to fail but got: " ++
            show tax
  where
    errOrTax = Indent.runIndent $
        Parsec.runParserT (pTaxonomy <* Parsec.eof) () "<test>" src
