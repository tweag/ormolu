module ReludeChainExample where

import Relude

resolveValue =
  primarySource
    ?: secondarySource
    ?: tertiarySource
    ?: quaternarySource
    ?: finalFallbackValue
