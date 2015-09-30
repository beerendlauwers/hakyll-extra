{-# LANGUAGE OverloadedStrings #-}
module Hakyll.Translation.Examples.Directories where

import           Hakyll
import           Hakyll.Translation.Format.Metadata (createTranslationContextFromMetaData)
import           Hakyll.Core.Identifier.Pattern.Extra (fillPattern)
import           Data.Monoid ((<>))
import           Control.Monad          (forM_)
import           System.FilePath        (takeFileName)

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
        translations <- createTranslationContextFromMetaData "translations/*/*" lang
        -- Then, we add them to our default context.
        let langCtx = postCtx <> translations
        -- The fillPattern will change the pattern to something like "content/nl/*".
        match (fillPattern "content/*/*" lang) $ do
            -- We will place a file in a diretory called $lang$
            route $ customRoute ((\f -> lang ++ "/" ++ f) . takeFileName . toFilePath)
            compile $ do
                getResourceBody
                    >>= applyAsTemplate langCtx
                    >>= loadAndApplyTemplate "templates/html.html" langCtx
                    >>= relativizeUrls
                
        match "templates/*" $ compile templateCompiler
        
--------------------------------------------------------------------------------
postCtx :: Context String
postCtx = defaultContext