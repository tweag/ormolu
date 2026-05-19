{-# LANGUAGE ExplicitLevelImports #-}

import {-# SOURCE #-} safe qualified A splice as QA hiding (a, b, c, d, e, f)
import quote qualified B as QB
import qualified C splice as SC
import Data.ByteString (e)
import Data.ByteString.Lazy quote (d)
import splice Data.Text (a, b, c)
