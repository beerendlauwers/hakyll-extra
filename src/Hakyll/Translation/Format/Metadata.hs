module Hakyll.Translation.Format.Metadata (loadTranslationsFromMetadata,
                                           collectMetadataTranslations,
                                           createTranslationContextFromMetaData,
                                           loadAllTranslationsFromMetadata) where

import Hakyll
import Hakyll.Core.Identifier.Pattern.Extra (fillPattern)
import Hakyll.Translation.Types
import Hakyll.Translation (translationToContext)
import qualified Data.Map  as M
import qualified Data.List as L
import Control.Monad (forM)

-- | Given a pattern, gets all the metadata values from every item that matches the pattern.
loadTranslationsFromMetadata :: (MonadMetadata m) => Pattern -> Language -> m ([(Identifier, Metadata)])
loadTranslationsFromMetadata p lang = do
    metadataList <- getAllMetadata (fillPattern p lang)
    return metadataList
    
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
createTranslationContextFromMetaData :: (MonadMetadata m) => Pattern -> Language -> m (Context a)
createTranslationContextFromMetaData pattern lang = do
 translationFiles <- loadTranslationsFromMetadata pattern lang
 let translations = translationToContext . collectMetadataTranslations . map snd $ translationFiles
 return translations

-- | Given a pattern and a list of languages, will produce a list of (language,translations) tuples.
loadAllTranslationsFromMetadata :: (MonadMetadata m) => Pattern -> [Language] -> m ([(Language,[Translation])])
loadAllTranslationsFromMetadata p langs = forM langs $ \lang -> do
 translations <- loadTranslationsFromMetadata p lang
 return (lang, collectMetadataTranslations . map snd $ translations)
    