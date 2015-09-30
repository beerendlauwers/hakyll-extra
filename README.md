# hakyll-extra
A bunch of extra functionality for Hakyll-based sites.

This repo can built with [Stack](https://github.com/commercialhaskell/stack#readme). You might have to edit the `stack.yaml` file if you're on a different LTS.

## Available functions

### Hakyll.Core.Identifier.Pattern.Extra

* `fillPattern :: Pattern -> String -> Pattern`: Matches the first * it encounters and leaves the rest alone.
* `fillPatterns :: Pattern -> [String] -> Pattern`: Matches as many * as there are in `xs` and leaves the rest alone.

### Hakyll.Core.Rules.Extra

* `staticBehavior :: Rules ()`: Copies a file into the final site.
* `compileStatic :: [Pattern] -> Rules ()`: Shorthand notation for matching on a list of patterns and compiling them with 'staticBehaviour'.

### Hakyll.Core.Util.JSON

* `renderToJSON :: ToJSON a => a -> String`: Produces a String that is valid JSON (can be copy-pasted into a browser and parsed).

### Hakyll.Web.Template.Context.Extra

* `optionalField :: String -> (Item a -> Maybe (Compiler String)) -> Context a`: See [commit by @wrengr](https://github.com/wrengr/hakyll/commit/558bc6d1efe86dc9ad501eaa100c0af020e2ef2a).
* `relativizeUrl :: Context a`: Allows you to call `$relativizeUrl("/url/to/relativize")$` in a template. [See this blogpost for more details](http://beerendlauwers.be/posts/2015-09-21-hakylls-functionfield.html).
* `applyToTemplate :: Context a`: A `functionField` that takes a template path as its first argument, and an unlimited number of named arguments. It will then load the template and pass the named arguments as template variables. [See the definition for a usage example.](https://github.com/beerendlauwers/hakyll-extra/blob/master/src/Hakyll/Web/Template/Context/Extra.hs)

### Hakyll.Translation

* `translationToContext :: [Translation] -> Context a`: Given a list of translations, builds up a context of fields, one for each translation. The metadata key name is appended with "translation.".
* `translationsToContextFunctionField :: [(Language,[Translation])] -> Context a`: Given a list of lists of translations, produces a `functionField` that expects a language and a translation key. You can use it in a template like this: `$translationFor("language","key")$`

### Hakyll.Translation.Format.Metadata

* `loadTranslationsFromMetadata :: (MonadMetadata m) => Pattern -> Language -> m ([(Identifier, Metadata)])`: Given a pattern, gets all the metadata values from every item that matches the pattern.
*  `collectMetadataTranslations :: [Metadata] -> [Translation]`: Given a list of metadata, appends them all together and places them in a map-like structure.
*  `createTranslationContextFromMetaData :: (MonadMetadata m) => Pattern -> Language -> m (Context a)`: Given a pattern and a language, will produce a Context with translations in them for that language. See [Hakyll.Translation.Examples.Directories](https://github.com/beerendlauwers/hakyll-extra/blob/master/src/Hakyll/Translation/Examples/Directories.hs) for a complete example.
*  `loadAllTranslationsFromMetadata :: (MonadMetadata m) => Pattern -> [Language] -> m ([(Language,[Translation])])`: Given a pattern and a list of languages, will produce a list of (language,translations) tuples. See [Hakyll.Translation.Examples.AllLanguages](https://github.com/beerendlauwers/hakyll-extra/blob/master/src/Hakyll/Translation/Examples/AllLanguages.hs) for a complete example.
*  
### Hakyll.Translation.Examples

* `AllLanguages.hs`: Example on how to use the `translationsToContextFunctionField` functionality.
* `Directories.hs`: Example on how to use the `createTranslationContextFromMetaData` functionality.
* `ToJavascript.hs` and `processTranslation.js`: Example on how to expose translations to Javascript.
