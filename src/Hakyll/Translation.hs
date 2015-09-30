module Hakyll.Translation where

import Hakyll
import Hakyll.Translation.Types
import Data.Monoid (mappend)

-- | Given a list of translations, builds up a context of fields, one for each translation.
-- | The metadata key name is appended with "translation.".
translationToContext :: [Translation] -> Context a
translationToContext = foldl mappend missingField . map (\(k,v) -> constField ("translation." ++ k) v)

-- | Given a list of lists of translations, produces a $functionField$ that expects a language and a translation key.
-- You can use it in a template like this: 
--
-- > $translationFor("language","key")$
translationsToContextFunctionField :: [(Language,[Translation])] -> Context a
translationsToContextFunctionField trs = functionField "translationFor" $ \args item ->
    case args of
        [language,key] -> case lookup language trs of
                            (Just translations) -> case lookup key translations of
                                                    (Just translation) -> return translation
                                                    otherwise          -> fail $ "translation for " ++ key ++ " not found in translation set " ++ language ++ "."
                            otherwise           -> fail $ "translation set " ++ language ++ " not found."
        _ -> fail "translationFor expects a language and a translation key, like so: $translationFor(\"language\",\"key\")$"
