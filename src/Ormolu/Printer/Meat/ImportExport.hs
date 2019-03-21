{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Rendering of import and export lists.

module Ormolu.Printer.Meat.ImportExport
  ( p_hsmodExports
  , p_hsmodImport
  )
where

import Control.Monad
import GHC hiding (GhcPs, IE)
import HsImpExp (IE (..))
import Language.Haskell.GHC.ExactPrint.Types
import Ormolu.Printer.Combinators
import Ormolu.Printer.Meat.Common

p_hsmodExports :: [LIE GhcPs] -> R ()
p_hsmodExports xs = do
  parens . velt $ withSep comma (located' p_lie) xs

p_hsmodImport :: ImportDecl GhcPs -> R ()
p_hsmodImport ImportDecl {..} = line . velt' $
  [ do txt "import "
       when ideclSource $
         txt "{-# SOURCE #-} "
       when ideclSafe $
         txt "safe "
       when ideclQualified $
         txt "qualified "
       case ideclPkgQual of
         Nothing -> return ()
         Just slit -> do
           atom slit
           space
       located ideclName atom
       case ideclAs of
         Nothing -> return ()
         Just l -> do
           txt " as "
           located l atom
       case ideclHiding of
         Nothing -> return ()
         Just (hiding, _) ->
           when hiding $
             txt " hiding"
  ] ++ (case ideclHiding of
          Nothing -> []
          Just (_, l) ->
            [ inci . locatedVia Nothing l $
                parens . velt . withSep comma (located' p_lie)
            ])

p_lie :: IE GhcPs -> R ()
p_lie = \case
  IEVar l1 -> located l1 p_ieWrappedName
  IEThingAbs l1 -> located l1 p_ieWrappedName
  IEThingAll l1 -> do
    located l1 p_ieWrappedName
    txt " (..)"
  IEThingWith l1 w xs _ -> velt'
    [ located l1 p_ieWrappedName
    , inci $ do
        p_ieWildcard w
        parens . velt $ withSep comma (located' p_ieWrappedName) xs
    ]
    -- XXX I have no idea what field labels are in this context.
    -- parens . velt $ withSep comma (located' p_FieldLbl) fls
  IEModuleContents l1 -> located l1 p_hsmodName
  -- XXX I have no idea what these things are for.
  IEGroup _ _ -> return ()
  IEDoc _ -> return ()
  IEDocNamed _ -> return ()
