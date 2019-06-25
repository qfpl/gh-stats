module GitHub.Lens.Helpers where

import           Control.Lens (LensRules, lensField, lensRules, mappingNamer,
                               (&), (.~))

-- | LensRules that map field names directly to lens names. Used to remap field names and lens names
-- in @GitHub.Lens@.
--
-- > makeLensesWith rulez ''Foo
rulez
  :: LensRules
rulez =
  lensRules & lensField .~ mappingNamer pure
