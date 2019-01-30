{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Pretty-printer for Haskell AST.

module Ormolu.Printer
  ( printModule )
where

import Data.Char (isAlphaNum)
import Data.Text (Text)
import FieldLabel (FieldLbl (..))
import GHC hiding (GhcPs, IE)
import HsImpExp (IE (..))
import Language.Haskell.GHC.ExactPrint.Types
import Module (Module (..))
import OccName (OccName (..))
import Ormolu.Printer.Combinators
import Outputable (Outputable (..), showSDocUnsafe)
import RdrName (RdrName (..), rdrNameOcc)
import SrcLoc (combineSrcSpans)

-- | Render a module.

printModule
  :: Anns                       -- ^ Annotations
  -> ParsedSource               -- ^ Parsed source
  -> Text                       -- ^ Resulting rendition
printModule anns src =
  runR False (p_HsModule src) anns

p_HsModule :: ParsedSource -> R ()
p_HsModule loc@(L moduleSpan hsModule) = do
  -- NOTE If span of exports in multiline, the whole thing is multiline.
  -- This is especially important because span of module itself always seems
  -- to have length zero, so it's not reliable for layout selection.
  let spn =
        case hsmodExports hsModule of
          Nothing -> moduleSpan
          Just (L exportsSpan _) -> combineSrcSpans moduleSpan exportsSpan
  locatedVia spn loc $ \HsModule {..} ->
    case hsmodName of
      Nothing -> pure ()
      Just hsmodName' -> line . velt' $
        [ located hsmodName' p_hsmodName ] ++
        (case hsmodExports of
           Nothing -> []
           Just hsmodExports' ->
             [ inci (locatedVia spn hsmodExports' p_hsmodExports)
             ])
        ++ [ txt "where"
           ]

p_hsmodName :: ModuleName -> R ()
p_hsmodName mname = do
  txt "module "
  atom mname

p_hsmodExports :: [LIE GhcPs] -> R ()
p_hsmodExports xs = do
  parens . velt $ withSep comma (located' p_lie) xs

p_lie :: IE GhcPs -> R ()
p_lie = \case
  IEVar l1 -> located l1 p_ieWrappedName
  IEThingAbs l1 -> located l1 p_ieWrappedName
  IEThingAll l1 -> do
    located l1 p_ieWrappedName
    txt " (..)"
  IEThingWith l1 w xs fls -> do
    located l1 p_ieWrappedName
    space
    p_ieWildcard w
    parens . velt $ withSep comma (located' p_ieWrappedName) xs
    parens . velt $ withSep comma (located' p_FieldLbl) fls
  IEModuleContents l1 -> located l1 p_hsmodName
  -- XXX I have no idea what these things are for.
  IEGroup _ _ -> return ()
  IEDoc _ -> return ()
  IEDocNamed _ -> return ()

p_ieWrappedName :: IEWrappedName RdrName -> R ()
p_ieWrappedName = \case
  IEName l2 -> located l2 p_rdrName
  IEPattern l2 -> located l2 $ \x -> do
    txt "pattern "
    p_rdrName x
  IEType l2 -> located l2 $ \x -> do
    txt "type "
    p_rdrName x

p_rdrName :: RdrName -> R ()
p_rdrName x = opParens (rdrNameOcc x) $ case x of
  Unqual occName -> atom occName
  Qual mname occName -> p_qualName mname occName
  Orig (Module _ mname) occName -> p_qualName mname occName
  Exact name -> atom name

p_FieldLbl :: FieldLbl RdrName -> R ()
p_FieldLbl (FieldLabel x _ _) = atom x

p_qualName :: ModuleName -> OccName -> R ()
p_qualName mname occName = do
  atom mname
  txt "."
  atom occName

p_ieWildcard :: IEWildcard -> R ()
p_ieWildcard = \case
  NoIEWildcard -> return ()
  IEWildcard n -> parens (atom n)

----------------------------------------------------------------------------
-- Helpers

-- | Put parentheses around the second argument if the 'Outputable' thing
-- consists only of punctuation characters.

opParens :: Outputable a => a -> R () -> R ()
opParens x m =
  if all (not . isAlphaNum) (showSDocUnsafe (ppr x))
    then txt "(" >> m >> txt ")"
    else m
