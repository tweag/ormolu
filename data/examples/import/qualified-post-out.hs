{-# LANGUAGE ImportQualifiedPost #-}

import Data.Text qualified (a, b, c)
import Data.Text qualified hiding (a, b, c)
import Data.Text qualified as T
