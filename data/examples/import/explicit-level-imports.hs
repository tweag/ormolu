{-# LANGUAGE ExplicitLevelImports #-}

import splice Data.Text (a, b, c)
import Data.ByteString.Lazy quote (d)
import Data.ByteString (e)
import {-# SOURCE #-} safe qualified A splice as QA hiding (a, b, c, d, e, f)
import quote qualified B as QB
import qualified C splice as SC
import A splice
import qualified D splice
