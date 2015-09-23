module Hakyll.Translation where

import Hakyll
import Hakyll.Translation.Types
import Data.Monoid (mappend)

-- | Given a list of translations, builds up a context of fields, one for each translation.
-- | The metadata key name is appended with "translation.".
translationToContext :: [Translation] -> Context a
translationToContext = foldl mappend missingField . map (\(k,v) -> constField ("translation." ++ k) v)