{-# LANGUAGE PackageImports #-}

import "base" Control.Applicative (Alternative, (<|>))
import "base" Data.Maybe (Maybe (Nothing), maybe)
import "base" System.IO (IO)
import "yaya" Yaya.Fold (ana, cata)
import "base" Prelude ((+))
