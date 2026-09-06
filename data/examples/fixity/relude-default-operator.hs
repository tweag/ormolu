module ReludeExample where

import Relude

config =
  lookupOptionalSetting environment
    ?: defaultConfigurationValue
