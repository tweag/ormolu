import A hiding
  ( foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  , foobarbazqux
  )

import GHC.Types.Name hiding ()

import {-# SOURCE #-} safe qualified Module as M hiding (a, b, c, d, e, f)
