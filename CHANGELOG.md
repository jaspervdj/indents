Changelog
=========

- 0.5.0.0 (2018-06-22)
    * Add the `Text.Parsec.Indent.Explicit` module

- 0.4.0.1 (2017-12-26)
    * Bump `tasty` and `tasty-hunit` dependencies

- 0.4.0.0 (2016-12-22)
    * Improved error messages
    * Added a simple test suite
    * Use `ReaderT` stack instead of `State`
    * Add `IndentParserT` in addition to `IndentParser`
    * Remove dependency on `concatenative`
    * Add `topLevel`, `notTopLevel` functions
    * Add `runIndentParserT`, `runIndentParser` convenience functions
