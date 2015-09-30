{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Translation.Examples.ToJavascript where

import           Hakyll
import           Hakyll.Translation.Format.Metadata (loadTranslationsFromMetadata,collectMetadataTranslations)
import           Hakyll.Core.Util.JSON
import           Data.Monoid ((<>))
import           Control.Monad          (forM_)

-- | Some example languages.
langs = ["nl","fr"];

-- | An example that illustrates how to use the translation functionality in "Hakyll.Translation.Format.Metadata".
exampleDirectories :: IO ()
exampleDirectories = hakyll $ do

    -- Note that we don't route these anywhere, we just want them to be known so they can be loaded later on.
    match "translations/*/*" $ do
         compile $ copyFileCompiler
    
    -- From here on in, we generate the files for each language.
    forM_ langs $ \lang -> do
        -- Here, we create the translations.
        translations <- loadTranslationsFromMetadata "translations/*/*" lang
        -- Then, we use them to generate translation files in Javascript.
        -- See the note below how these would be included.
        create [fromFilePath (getTranslationFilePath lang)] $ do
            route   idRoute
            compile $ do
                makeItem (renderTranslationsToJS lang translations)
        
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = translationFilePaths <> allLanguages <> defaultContext

-- This is where we'll place the translations for a particular language.
getTranslationFilePath lang = "js/" ++ lang ++ "/translations.js"

-- The translation file will just be a Javascript file with this single line in it.
renderTranslationsToJS :: String -> [(Identifier, Metadata)] -> String
renderTranslationsToJS lang ts = "var gRawTranslation_" ++ lang ++ " = " ++ (renderToJSON . collectMetadataTranslations . map snd $ ts) 

-- This is used in the template snippet below.
translationFilePaths = listField "translationFiles" (field "filePath" $ return . getTranslationFilePath . itemBody) (sequence (map makeItem langs))

-- This is used in the processTranslation.js file.
allLanguages = constField "js.allLanguages" (renderToJSON langs)

{- 

// The generated translation files could be included by using this template file:

$for(translationFiles)$
    <script src="/$filePath$"></script>
$endfor$
<script src="/js/processTranslations.js"></script> // This file is also available in this repository, same directory.

// After inclusion, it's possible to use the hakyllTranslate Javascript function to access the translations.

-}