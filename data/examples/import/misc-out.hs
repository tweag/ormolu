import A hiding
  ( foobarbazqux,
  )
import {-# SOURCE #-} safe qualified Module as M hiding (a, b, c, d, e, f)
import GHC.Types.Name hiding ()
