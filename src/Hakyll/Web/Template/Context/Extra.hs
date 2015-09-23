module Hakyll.Web.Template.Context.Extra where

import Hakyll
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