module Main
    ( main
    ) where

import qualified Test.Tasty               as Tasty
import qualified Text.Parsec.Indent.Tests

main :: IO ()
main = Tasty.defaultMain Text.Parsec.Indent.Tests.tests
