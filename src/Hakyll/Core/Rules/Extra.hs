module Hakyll.Core.Rules.Extra where

import           Hakyll

-- |Copies a file into the final site.
staticBehavior :: Rules ()
staticBehavior = do
  route   idRoute
  compile copyFileCompiler

-- |Shorthand notation for matching on a list of patterns and compiling them with 'staticBehaviour'.
compileStatic :: [Pattern] -> Rules ()
compileStatic (x:xs) = match (foldr (.||.) x xs) staticBehavior