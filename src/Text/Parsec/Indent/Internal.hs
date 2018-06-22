-- | Some internals.  They are exposed for your convenience but can change in
-- between patch releases.
module Text.Parsec.Indent.Internal
    ( Indentation (..)
    , prettyIndentation
    , prettyLine
    ) where

-- | We use our own position type that doesn't require a 'SourceName'.
data Indentation = Indentation
    { iLine   :: !Int
    , iColumn :: !Int
    } deriving (Show)

prettyIndentation :: Indentation -> String
prettyIndentation i = case iColumn i of
    1 -> "top-level indentation"
    c -> show (c - 1) ++ "-column indentation"

prettyLine :: Indentation -> String
prettyLine = show . iLine
