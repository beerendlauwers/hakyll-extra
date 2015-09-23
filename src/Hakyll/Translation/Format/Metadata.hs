module Hakyll.Translation.Format.Metadata (loadTranslationsFromMetadata,
                                           collectMetadataTranslations,
                                           createTranslationContextFromMetaData) where

import Hakyll
import Hakyll.Core.Identifier.Pattern.Extra (fillPattern)
import Hakyll.Translation.Types
import Hakyll.Translation (translationToContext)
import qualified Data.Map  as M
import qualified Data.List as L

-- | Given a pattern, gets all the metadata values from every item that matches the pattern.
loadTranslationsFromMetadata :: Pattern -> Language -> Compiler (Item [(Identifier, Metadata)])
loadTranslationsFromMetadata p lang = do
    metadataList <- getAllMetadata (fillPattern p lang)
    makeItem metadataList
    
-- | Given a list of metadata, appends them all together and places them in a map-like structure.
collectMetadataTranslations :: [Metadata] -> [Translation]
collectMetadataTranslations = foldr L.union [] . map M.toList

-- | Given a pattern and a language, will produce a Context with translations in them.
-- You can use it like this:
-- 
-- > forM_ langs $ \lang -> do
-- >      match "path/to/content/*"  $ do
-- >         compile $ do
-- >             translations <- createTranslationContextFromMetaData "translations/*/*" lang
-- >             let langCtx = postCtx <> translations
-- >             loadAndApplyTemplate "templates/default.html" langCtx
--
-- See "Hakyll.Translation.Examples.Directories" for a complete example.
createTranslationContextFromMetaData :: Pattern -> Language -> Compiler (Context a)
createTranslationContextFromMetaData pattern lang = do
 translationFiles <- loadTranslationsFromMetadata pattern lang
 let translations = translationToContext . collectMetadataTranslations . map snd . itemBody $ translationFiles
 return translations