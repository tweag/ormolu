module GHC
  ( module X,
    ParsedSource,
  )
where

import ApiAnnotation as X
import BasicTypes as X
import HsBinds as X
import HsDecls as X
import HsDoc as X
import HsExpr as X
import HsExtension as X
import HsImpExp as X
import HsInstances as X ()
import HsLit as X
import HsPat as X
import HsSyn as X
import Module as X
import RdrName as X
import SrcLoc as X

type ParsedSource = Located (HsModule GhcPs)
