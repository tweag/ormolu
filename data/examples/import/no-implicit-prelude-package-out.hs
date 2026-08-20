{-# LANGUAGE PackageImports #-}
{-# LANGUAGE NoImplicitPrelude #-}

import "base" Control.Applicative (Alternative, (<|>))
import "base" Data.Maybe (Maybe (Nothing), maybe)
import "base" Prelude ((+))
import "base" System.IO (IO)
import "yaya" Yaya.Fold (ana, cata)
