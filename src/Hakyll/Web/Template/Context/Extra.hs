module Hakyll.Web.Template.Context.Extra where

import Hakyll
import Data.Monoid                    (mappend)
import Data.List                      (isPrefixOf)
import Control.Applicative            (Alternative (..))

--------------------------------------------------------------------------------
-- | Constructs a new 'field' which can be used with the @$if()$@
-- template macro, as well as being used as a regular field. If the
-- function returns @Nothing@ then @$if()$@ evaluates the
-- @$else$@-branch, and trying to use the field as a string will
-- generate an error; otherwise it evaluates the then-branch, and
-- the field can safely be used as usual.
optionalField
    :: String
    -> (Item a -> Maybe (Compiler String))
    -> Context a
optionalField key f = field key $ \i ->
    case f i of
    Nothing    -> empty
    Just value -> value

-- | A 'Hakyll.Web.Template.Context.functionField' that does the same as 'Hakyll.Web.RelativizeUrls.relativizeUrls', but can be used anywhere in a template.
-- You can call it in a template like this:
--
-- > $relativizeUrl("/url/to/relativize")$
--
-- You can also refer to other fields. 
-- Let's say that @pathToImages@ is another Hakyll field that produces @"\/hi\/there"@.
-- We can then do the following:
--
-- > $relativizeUrl(pathToImages)$
relativizeUrl :: Context a
relativizeUrl = functionField "relativizeUrl" $ \args item ->
    case args of
        [k] -> do   route <- getRoute $ itemIdentifier item
                    return $ case route of
                        Nothing -> k
                        Just r -> rel k (toSiteRoot r)
        _   -> fail "relativizeUrl only needs a single argument"
     where
        isRel x = "/" `isPrefixOf` x && not ("//" `isPrefixOf` x)
        rel x root = if isRel x then root ++ x else x
        
-- | A 'Hakyll.Web.Template.Context.functionField' that takes a template path as its first argument, and an unlimited number of named arguments.
-- It will then load the template and pass the named arguments as template variables.
-- You can call it in a template like this:
--
-- > $applyToTemplate("templates/testing.html","element","testing")$
--
-- @"templates/testing.html"@ will now have access to the @$element$@ variable, which will have the value @"testing"@.
--
-- Multiple arguments are possible:
--
-- > $applyToTemplate("templates/testing.html","element","testing","argumentTwo","arg2")$
--
-- @"templates/testing.html"@ will now have access to both the @$element$@ and the @$argumentTwo$@ variables.
applyToTemplate :: Context a
applyToTemplate = functionField "applyToTemplate" $ \args item ->
    case args of
        (template:arguments) -> let indexedArguments = zip arguments [0..] -- Gives us [("argumentName1",0),("argumentName1Value",1),("argumentName2",2),("argumentName2Value",3)]
                                    names = map fst . filter (even . snd) $ indexedArguments -- Gives us ["argumentName1","argumentName2"]
                                    values = map fst . filter (not . even . snd) $ indexedArguments -- Gives us ["argumentName1Value","argumentName2Value"]
                                    fields = map (\(name,arg) -> constField name arg) (zip names values)
                                    context = foldl1 mappend fields
                                in do result <- loadAndApplyTemplate (fromFilePath template) context item
                                      return (itemBody result)