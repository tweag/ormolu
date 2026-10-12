{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}

-- | Rendering of type synonym declarations.
module Ormolu.Printer.Meat.Declaration.Type
  ( p_synDecl,
  )
where

import Data.Choice (pattern Is, pattern Isn't)
import GHC.Hs.Extension
import GHC.Hs.Type
import GHC.Parser.Annotation
import GHC.Types.Fixity
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common
import Ormolu.Printer.Meat.Type

p_synDecl ::
  -- | Type constructor
  LocatedN RdrName ->
  -- | Fixity
  LexicalFixity ->
  -- | Type variables
  LHsQTyVars GhcPs ->
  -- | RHS of type declaration
  LHsType GhcPs ->
  R ()
p_synDecl name fixity HsQTvs {..} t = do
  txt "type"
  space
  switchLayout (getLocA name : map getLocA hsq_explicit) $
    p_infixDefHelper
      ( case fixity of
          Infix -> Is #infixStyle
          _ -> Isn't #infixStyle
      )
      (Is #indentArgs)
      (p_rdrName name)
      (map (located' p_hsTyVarBndr) hsq_explicit)
  inci $ do
    space
    txt "="
    if hasDocStrings (unLoc t)
      then newline
      else breakpoint
    located t p_hsType
