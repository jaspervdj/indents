-- |
-- = Introduction
--
-- The 'Text.Parsec.Indent' module provides an /implicit/ indentation parser.
-- For complex parsers split over many functions, this requires a good
-- understanding from the programmer which indentation is being referenced.
--
-- This module fixes that problem by explicitly passing around indentation as a
-- first-class value.  This commonly makes complex parsers less concise but
-- easier to understand.
--
-- = The problem with reference indentation
--
-- Many functions from that module (@indented@, @checkIndent@,
-- @sameOrIndented@...) use a /reference indentation/.  This
-- /reference indentation/ is stored in the @IndentParserT@ type.  It can be set
-- by calling @withPos@, but also by other functions such as @block@.
--
-- Consider the following code snippet:
--
-- > import qualified Text.Parsec.Indent as I
-- >
-- > p = I.withPos $ do
-- >   foo
-- >   I.block $ I.checkIndent >> bar
--
-- @I.checkIndent@, in this case, will always succeed, since we are comparing
-- the current indentation to the implicit reference indentation set by
-- @I.block@, not by @I.withPos@, and there is no way to do the latter.
--
-- = Explicit indentation
--
-- This module makes indentation first-class rather than implicit, so we can
-- provide a good implementation without relying on @withPos@:
--
-- > import qualified Text.Parsec.Indent as I
-- > import qualified Text.Parsec.Indent.Explicit as EI
-- >
-- > p = do
-- >   indentation <- EI.indentation
-- >   foo
-- >   I.block $ EI.checkIndent indentation >> bar
--
-- In order to preserve backwards-compatibility, the names in this module are
-- chosen to match their counterparts in "Text.Parsec.Indent".
module Text.Parsec.Indent.Explicit
    ( -- * Indentation type
      Indentation

      -- * Obtaining a reference implementation
    , indentation

      -- * Indentation-based parser combinators
    , indented
    , sameOrIndented
    , same
    , block
    , checkIndent
    , topLevel
    , notTopLevel
    ) where

import           Control.Monad               (unless, when)
import           Text.Parsec
import           Text.Parsec.Indent.Internal

-- | Obtain the current indentation, to be used as a reference later.
indentation :: Monad m => ParsecT s u m Indentation
indentation = do
    pos <- getPosition
    return $! Indentation {iLine = sourceLine pos, iColumn = sourceColumn pos}

-- | Parses only when indented past the level of the reference
indented
    :: (Monad m, Stream s m z)
    => Indentation  -- ^ Reference indentation
    -> ParsecT s u m ()
indented ref = do
    pos <- indentation
    when (iColumn pos <= iColumn ref) $ unexpected (prettyIndentation pos)

-- | Parses only when indented past the level of the reference or on the same line
sameOrIndented
    :: (Monad m, Stream s m z)
    => Indentation  -- ^ Reference indentation
    -> ParsecT s u m ()
sameOrIndented ref = do
    -- This is equal to 'same <|> indented' but gives a cleaner error message.
    pos <- indentation
    when (iColumn pos <= iColumn ref && iLine pos /= iLine ref) $
        unexpected (prettyIndentation pos)

-- | Parses only on the same line as the reference
same
    :: (Monad m, Stream s m z)
    => Indentation  -- ^ Reference indentation
    -> ParsecT s u m ()
same ref = do
    pos <- indentation
    when (iLine pos /= iLine ref) $ unexpected "line break"

-- | Parses a block of lines at the same indentation level starting at the
-- current position
block
    :: (Monad m, Stream s m z)
    => ParsecT s u m a
    -> ParsecT s u m [a]
block p = do
    ref <- indentation
    many1 (checkIndent ref >> p)

-- | Ensures the current indentation level matches that of the reference
checkIndent
    :: (Monad m, Stream s m z)
    => Indentation  -- ^ Reference indentation
    -> ParsecT s u m ()
checkIndent ref = do
    pos <- indentation
    when (iColumn pos /= iColumn ref) $
        (<?> prettyIndentation ref ++ " (started at line " ++ prettyLine ref ++ ")")
        (unexpected $ prettyIndentation pos)

-- | Ensures that there is no indentation.
topLevel
    :: (Monad m, Stream s m z)
    => ParsecT s u m ()
topLevel = do
    pos <- indentation
    unless (iColumn pos == 1) $ unexpected "indentation"

-- | Ensures that there is at least some indentation.
notTopLevel
    :: (Monad m, Stream s m z)
    => ParsecT s u m ()
notTopLevel = do
    pos <- indentation
    when (iColumn pos == 1) $ unexpected "top-level"
