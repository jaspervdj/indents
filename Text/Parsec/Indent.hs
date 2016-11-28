{-# LANGUAGE FlexibleContexts #-}
module Text.Parsec.Indent (
    -- $doc
    
    -- * Types
    IndentParser, runIndent,
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
import Text.Parsec hiding (State)
import Text.Parsec.Pos
import Text.Parsec.Token
import Control.Monad.State

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

-- | Indentation sensitive parser type. Usually @ m @ will
--   be @ Identity @ as with any @ ParsecT @
type IndentParser s u a = ParsecT s u (State SourcePos) a
    
-- | @ 'withBlock' f a p @ parses @ a @
--   followed by an indented block of @ p @
--   combining them with @ f @
withBlock :: (Stream s (State SourcePos) z) => (a -> [b] -> c) ->
    IndentParser s u a -> IndentParser s u b -> IndentParser s u c
withBlock f a p = withPos $ do
    r1 <- a
    r2 <- option [] (indented >> block p)
    return (f r1 r2)

-- | Like 'withBlock', but throws away initial parse result
withBlock' :: (Stream s (State SourcePos) z) =>
    IndentParser s u a -> IndentParser s u b -> IndentParser s u [b]
withBlock' = withBlock (flip const)

-- | Parses only when indented past the level of the reference
indented :: (Stream s (State SourcePos) z) => IndentParser s u ()
indented = do
    pos <- getPosition
    s <- get
    if sourceColumn pos <= sourceColumn s then parserFail "not indented" else do
        put $ setSourceLine s (sourceLine pos)
        return ()

-- | Parses only when indented past the level of the reference or on the same line
sameOrIndented :: Stream s (State SourcePos) z => IndentParser s u ()
sameOrIndented = same <|> indented

-- | Parses only on the same line as the reference
same :: (Stream s (State SourcePos) z) => IndentParser s u ()
same = do
    pos <- getPosition
    s <- get
    if sourceLine pos == sourceLine s then return () else parserFail "over one line"
    
-- | Parses a block of lines at the same indentation level
block :: (Stream s (State SourcePos) z) => IndentParser s u a -> IndentParser s u [a]
block p = withPos $ do
    r <- many1 (checkIndent >> p)
    return r

-- | Parses using the current location for indentation reference
withPos :: (Stream s (State SourcePos) z) => IndentParser s u a -> IndentParser s u a
withPos x = do
    a <- get
    p <- getPosition
    r <- put p >> x
    put a >> return r

-- | Ensures the current indentation level matches that of the reference
checkIndent :: (Stream s (State SourcePos) z) => IndentParser s u ()
checkIndent = do
    s <- get
    p <- getPosition
    if sourceColumn p == sourceColumn s then return () else parserFail "indentation doesn't match"

-- | Run the result of an indentation sensitive parse
runIndent :: SourceName -> State SourcePos a -> a
runIndent s = flip evalState (initialPos s)

-- | '<+/>' is to indentation sensitive parsers what 'ap' is to monads
(<+/>) :: (Stream s (State SourcePos) z) =>
    IndentParser s u (a -> b) -> IndentParser s u a -> IndentParser s u b
a <+/> b = ap a (sameOrIndented >> b)

-- | '<-/>' is like '<+/>', but doesn't apply the function to the parsed value
(<-/>) :: (Stream s (State SourcePos) z) =>
    IndentParser s u a -> IndentParser s u b -> IndentParser s u a
a <-/> b = liftM2 const a (sameOrIndented >> b)

-- | Like '<+/>' but applies the second parser many times
(<*/>) :: (Stream s (State SourcePos) z) =>
    IndentParser s u ([a] -> b) -> IndentParser s u a -> IndentParser s u b
a <*/> b = ap a (many (sameOrIndented >> b))

-- | Datatype used to optional parsing
data Optional s u a = Opt a (IndentParser s u a)

-- | Like '<+/>' but applies the second parser optionally using the 'Optional' datatype
(<?/>) :: (Stream s (State SourcePos) z) =>
    IndentParser s u (a -> b) -> (Optional s u a) -> IndentParser s u b
(<?/>) a (Opt b c) = ap a (option b (sameOrIndented >> c))

-- | parses with surrounding brackets
indentBrackets :: (Stream s (State SourcePos) z) => GenTokenParser s u (State SourcePos) -> IndentParser s u a -> IndentParser s u a
indentBrackets lexer p = withPos $ return id <-/> symbol lexer "[" <+/> p <-/> symbol lexer "]"

-- | parses with surrounding angle brackets
indentAngles :: (Stream s (State SourcePos) z) => GenTokenParser s u (State SourcePos) -> IndentParser s u a -> IndentParser s u a
indentAngles lexer p = withPos $ return id <-/> symbol lexer "<" <+/> p <-/> symbol lexer ">"

-- | parses with surrounding braces
indentBraces :: (Stream s (State SourcePos) z) => GenTokenParser s u (State SourcePos) -> IndentParser s u a -> IndentParser s u a
indentBraces lexer p = withPos $ return id <-/> symbol lexer "{" <+/> p <-/> symbol lexer "}"

-- | parses with surrounding parentheses 
indentParens :: (Stream s (State SourcePos) z) => GenTokenParser s u (State SourcePos) -> IndentParser s u a -> IndentParser s u a
indentParens lexer p = withPos $ return id <-/> symbol lexer "(" <+/> p <-/> symbol lexer ")"
