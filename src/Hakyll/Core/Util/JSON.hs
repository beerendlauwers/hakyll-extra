module Hakyll.Core.Util.JSON where

import           Hakyll
import           Data.Aeson                    
import           Codec.Binary.UTF8.Generic     (toString)

-- | Produces a String that is valid JSON (can be copy-pasted into a browser and parsed).
renderToJSON :: ToJSON a => a -> String
renderToJSON = toString . encode