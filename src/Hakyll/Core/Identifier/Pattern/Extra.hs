module Hakyll.Core.Identifier.Pattern.Extra where

import Hakyll

-- | Matches the first * it encounters and leaves the rest alone.
--
-- > fillPattern "*/a/b/*" "test" == test/a/b/*
--
fillPattern :: Pattern -> String -> Pattern
fillPattern p s = fillPatterns p [s]

-- | Matches as many * as there are in 'xs' and leaves the rest alone.
--
-- > fillPatterns "*/a/b/*/*" ["test","c"] == test/a/b/c/*
--
fillPatterns :: Pattern -> [String] -> Pattern
fillPatterns p xs = fromGlob . show $ fromCaptures  p (xs ++ repeat "*")