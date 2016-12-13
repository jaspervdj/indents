{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Text.Parsec.Indent (
    -- $doc

    -- * Types
    IndentT, IndentParserT, IndentParser, runIndent,
    -- * Blocks
    withBlock, withBlock', block,
    -- * Indentation Checking
    indented, same, sameOrIndented, checkIndent, withPos,
    -- * Paired characters
    indentBrackets, indentAngles, indentBraces, indentParens,
    -- * Line Fold Chaining
    -- | Any chain using these combinators must used with 'withPos'
    (<+/>), (<-/>), (<*/>), (<?/>), Optional(..)
    ) where

import           Control.Monad          (ap, liftM2, when)
import           Control.Monad.Identity (Identity, runIdentity)
import           Control.Monad.Reader   (ReaderT, ask, local, runReaderT)
import           Text.Parsec
import           Text.Parsec.Token

-- $doc
-- A module to construct indentation aware parsers. Many programming
-- language have indentation based syntax rules e.g. python and Haskell.
-- This module exports combinators to create such parsers.
--
-- The input source can be thought of as a list of tokens. Abstractly
-- each token occurs at a line and a column and has a width. The column
-- number of a token measures is indentation. If t1 and t2 are two tokens
-- then we say that indentation of t1 is more than t2 if the column
-- number of occurrence of t1 is greater than that of t2.
--
-- Currently this module supports two kind of indentation based syntactic
-- structures which we now describe:
--
-- [Block] A block of indentation /c/ is a sequence of tokens with
-- indentation at least /c/.  Examples for a block is a where clause of
-- Haskell with no explicit braces.
--
-- [Line fold] A line fold starting at line /l/ and indentation /c/ is a
-- sequence of tokens that start at line /l/ and possibly continue to
-- subsequent lines as long as the indentation is greater than /c/. Such
-- a sequence of lines need to be /folded/ to a single line. An example
-- is MIME headers. Line folding based binding separation is used in
-- Haskell as well.

-- | We use our own 'Position' type that doesn't require a 'SourceName'.
data Pos = Pos
    { pLine   :: !Int
    , pColumn :: !Int
    } deriving (Show)

getCurrentPos :: Monad m => IndentParserT s u m Pos
getCurrentPos = do
    pos <- getPosition
    return $! Pos {pLine = sourceLine pos, pColumn = sourceColumn pos}

getReferencePos :: Monad m => IndentParserT s u m Pos
getReferencePos = ask

-- | Indentation transformer.
type IndentT m = ReaderT Pos m

-- | Indentation sensitive parser type. Usually @m@ will be 'Identity' as with
-- any 'ParsecT'.  In that case you can use the simpler 'IndentParser' type.
type IndentParserT s u m a = ParsecT s u (IndentT m) a

-- | A simplified 'IndentParserT'.
type IndentParser s u a = IndentParserT s u Identity a

-- | @ 'withBlock' f a p @ parses @ a @
--   followed by an indented block of @ p @
--   combining them with @ f @
withBlock
    :: (Monad m, Stream s (IndentT m) z)
    => (a -> [b] -> c)
    -> IndentParserT s u m a
    -> IndentParserT s u m b
    -> IndentParserT s u m c
withBlock f a p = withPos $ do
    r1 <- a
    r2 <- option [] (indented >> block p)
    return (f r1 r2)

-- | Like 'withBlock', but throws away initial parse result
withBlock'
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m a
    -> IndentParserT s u m b
    -> IndentParserT s u m [b]
withBlock' = withBlock (flip const)

-- | Parses only when indented past the level of the reference
indented
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m ()
indented = do
    pos <- getCurrentPos
    ref <- getReferencePos
    when (pColumn pos <= pColumn ref) (parserFail "not indented")

-- | Parses only when indented past the level of the reference or on the same line
sameOrIndented
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m ()
sameOrIndented = same <|> indented

-- | Parses only on the same line as the reference
same
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m ()
same = do
    pos <- getCurrentPos
    s <- getReferencePos
    if pLine pos == pLine s then return () else parserFail "over one line"

-- | Parses a block of lines at the same indentation level
block
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m a
    -> IndentParserT s u m [a]
block p = withPos $ do
    r <- many1 (checkIndent >> p)
    return r

-- | Parses using the current location for indentation reference
withPos
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m a
    -> IndentParserT s u m a
withPos x = do
    p <- getCurrentPos
    local (const p) x

-- | Ensures the current indentation level matches that of the reference
checkIndent
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m ()
checkIndent = do
    s <- getReferencePos
    p <- getCurrentPos
    if pColumn p == pColumn s then return () else parserFail "indentation doesn't match"

-- | Run the result of an indentation sensitive parse
runIndentT :: Monad m => IndentT m a -> m a
runIndentT i = runReaderT i (Pos 1 1)

-- | Simplified version of 'runIndentT'.
runIndent :: IndentT Identity a -> a
runIndent = runIdentity . runIndentT

-- | '<+/>' is to indentation sensitive parsers what 'ap' is to monads
(<+/>)
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m (a -> b)
    -> IndentParserT s u m a
    -> IndentParserT s u m b
a <+/> b = ap a (sameOrIndented >> b)

-- | '<-/>' is like '<+/>', but doesn't apply the function to the parsed value
(<-/>)
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m a
    -> IndentParserT s u m b
    -> IndentParserT s u m a
a <-/> b = liftM2 const a (sameOrIndented >> b)

-- | Like '<+/>' but applies the second parser many times
(<*/>)
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m ([a] -> b)
    -> IndentParserT s u m a
    -> IndentParserT s u m b
a <*/> b = ap a (many (sameOrIndented >> b))

-- | Datatype used to optional parsing
data Optional s u m a = Opt a (IndentParserT s u m a)

-- | Like '<+/>' but applies the second parser optionally using the 'Optional' datatype
(<?/>)
    :: (Monad m, Stream s (IndentT m) z)
    => IndentParserT s u m (a -> b)
    -> (Optional s u m a)
    -> IndentParserT s u m b
(<?/>) a (Opt b c) = ap a (option b (sameOrIndented >> c))

-- | parses with surrounding brackets
indentBrackets
    :: (Monad m, Stream s (IndentT m) z)
    => GenTokenParser s u (IndentT m)
    -> IndentParserT s u m a
    -> IndentParserT s u m a
indentBrackets lexer p = withPos $ return id <-/> symbol lexer "[" <+/> p <-/> symbol lexer "]"

-- | parses with surrounding angle brackets
indentAngles
    :: (Monad m, Stream s (IndentT m) z)
    => GenTokenParser s u (IndentT m)
    -> IndentParserT s u m a
    -> IndentParserT s u m a
indentAngles lexer p = withPos $ return id <-/> symbol lexer "<" <+/> p <-/> symbol lexer ">"

-- | parses with surrounding braces
indentBraces
    :: (Monad m, Stream s (IndentT m) z)
    => GenTokenParser s u (IndentT m)
    -> IndentParserT s u m a
    -> IndentParserT s u m a
indentBraces lexer p = withPos $ return id <-/> symbol lexer "{" <+/> p <-/> symbol lexer "}"

-- | parses with surrounding parentheses
indentParens
    :: (Monad m, Stream s (IndentT m) z)
    => GenTokenParser s u (IndentT m)
    -> IndentParserT s u m a
    -> IndentParserT s u m a
indentParens lexer p = withPos $ return id <-/> symbol lexer "(" <+/> p <-/> symbol lexer ")"
