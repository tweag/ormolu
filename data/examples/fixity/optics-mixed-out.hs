module OpticsExample where

import Optics

updated =
  record
    & fieldLens % subFieldLens .~ someValue
    & otherLens %~ someTransformationFunctionApplied
