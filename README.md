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

* `relativizeUrl :: Context a`: Allows you to call `$relativizeUrl("/url/to/relativize")$` in a template. [See this blogpost for more details](http://beerendlauwers.be/posts/2015-09-21-hakylls-functionfield.html).
