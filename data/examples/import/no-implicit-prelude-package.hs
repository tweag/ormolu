{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PackageImports #-}

import "base" System.IO (IO)
import "base" Prelude ((+))
import "yaya" Yaya.Fold (ana, cata)
import "base" Control.Applicative (Alternative, (<|>))
import "base" Data.Maybe (Maybe (Nothing), maybe)
