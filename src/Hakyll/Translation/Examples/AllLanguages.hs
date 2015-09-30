{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Translation.Examples.AllLanguages where

import           Hakyll
import           Hakyll.Translation.Format.Metadata (loadAllTranslationsFromMetadata)
import           Hakyll.Translation (translationsToContextFunctionField)
import           Hakyll.Core.Identifier.Pattern.Extra (fillPattern)
import           Data.Monoid ((<>))
import           System.FilePath        (takeFileName)

-- | Some example languages.
langs = ["nl","fr"];

-- | An example that illustrates how to use the translation functionality in "Hakyll.Translation.Format.Metadata".
exampleAllLanguages :: IO ()
exampleAllLanguages = hakyll $ do

    -- Note that we don't route these anywhere, we just want them to be known so they can be loaded later on.
    match "translations/*/*" $ do
         compile $ copyFileCompiler
         
    -- Here, we collect all the translations for all languages.
    allTranslations <- loadAllTranslationsFromMetadata "translations/*/*" langs
    
    -- Then, we produce a functionField that will look up a translation for a given language and key.
    let translationFunction = translationsToContextFunctionField allTranslations
    
    -- We add that to the context:
    let langCtx = postCtx <> translationFunction
    
    match "content/*" $ do
        route idRoute
        compile $ do
            getResourceBody
                >>= applyAsTemplate langCtx
                >>= loadAndApplyTemplate "templates/html.html" langCtx
                >>= relativizeUrls
                
    match "templates/*" $ compile templateCompiler
        
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = defaultContext