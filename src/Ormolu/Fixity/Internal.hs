{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Ormolu.Fixity.Internal
  ( OpName,
    pattern OpName,
    unOpName,
    occOpName,
    FixityDirection (..),
    FixityInfo (..),
    colonFixityInfo,
    defaultFixityInfo,
    FixityApproximation (..),
    defaultFixityApproximation,
    HackageInfo (..),
    FixityOverrides (..),
    defaultFixityOverrides,
    ModuleReexports (..),
    defaultModuleReexports,
    PackageFixityMap (..),
    ModuleFixityMap (..),
    FixityProvenance (..),
    FixityQualification (..),
    inferFixity,
  )
where

import Data.Binary (Binary)
import Data.Binary qualified as Binary
import Data.Binary.Get qualified as Binary
import Data.Binary.Put qualified as Binary
import Data.ByteString.Short (ShortByteString)
import Data.ByteString.Short qualified as SBS
import Data.Choice (Choice)
import Data.Choice qualified as Choice
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Debug.Trace (trace)
import Distribution.ModuleName (ModuleName)
import Distribution.Types.PackageName
import GHC.Data.FastString (fs_sbs)
import GHC.Generics (Generic)
import GHC.Types.Name (OccName (occNameFS))
import GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import Ormolu.Utils (ghcModuleNameToCabal)

-- | An operator name.
newtype OpName = MkOpName
  { -- | Invariant: UTF-8 encoded
    getOpName :: ShortByteString
  }
  deriving newtype (Eq, Ord, Binary)

-- | Convert an 'OpName' to 'Text'.
unOpName :: OpName -> Text
unOpName = T.decodeUtf8 . SBS.fromShort . getOpName

pattern OpName :: Text -> OpName
pattern OpName opName <- (unOpName -> opName)
  where
    OpName = MkOpName . SBS.toShort . T.encodeUtf8

{-# COMPLETE OpName #-}

-- | Convert an 'OccName to an 'OpName'.
occOpName :: OccName -> OpName
occOpName = MkOpName . fs_sbs . occNameFS

instance Show OpName where
  show = T.unpack . unOpName

instance IsString OpName where
  fromString = OpName . T.pack

-- | Fixity direction.
data FixityDirection
  = InfixL
  | InfixR
  | InfixN
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Binary)

-- | Fixity information about an infix operator. This type provides precise
-- information as opposed to 'FixityApproximation'.
data FixityInfo = FixityInfo
  { -- | Fixity direction
    fiDirection :: FixityDirection,
    -- | Precedence
    fiPrecedence :: Double
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Binary FixityInfo where
  put FixityInfo {..} = do
    Binary.put fiDirection
    Binary.putDoublele fiPrecedence

  get = do
    fiDirection <- Binary.get
    fiPrecedence <- Binary.getDoublele
    pure FixityInfo {..}

-- | Fixity info of the built-in colon data constructor.
colonFixityInfo :: FixityInfo
colonFixityInfo = FixityInfo InfixR 5

-- | Fixity that is implicitly assumed if no fixity declaration is present.
defaultFixityInfo :: FixityInfo
defaultFixityInfo = FixityInfo InfixL 9

-- | Approximation of fixity information that takes the uncertainty that can
-- arise from conflicting definitions into account.
data FixityApproximation = FixityApproximation
  { -- | Fixity direction if it is known
    faDirection :: Maybe FixityDirection,
    -- | Minimum precedence level found in the (maybe conflicting)
    -- definitions for the operator (inclusive)
    faMinPrecedence :: Double,
    -- | Maximum precedence level found in the (maybe conflicting)
    -- definitions for the operator (inclusive)
    faMaxPrecedence :: Double
  }
  deriving stock (Eq, Ord, Show, Generic)

instance Binary FixityApproximation where
  put FixityApproximation {..} = do
    Binary.put faDirection
    Binary.putDoublele faMinPrecedence
    Binary.putDoublele faMaxPrecedence

  get = do
    faDirection <- Binary.get
    faMinPrecedence <- Binary.getDoublele
    faMaxPrecedence <- Binary.getDoublele
    pure FixityApproximation {..}

-- | Gives the ability to merge two (maybe conflicting) definitions for an
-- operator, keeping the higher level of compatible information from both.
instance Semigroup FixityApproximation where
  FixityApproximation {faDirection = dir1, faMinPrecedence = min1, faMaxPrecedence = max1}
    <> FixityApproximation {faDirection = dir2, faMinPrecedence = min2, faMaxPrecedence = max2} =
      FixityApproximation
        { faDirection = dir',
          faMinPrecedence = min min1 min2,
          faMaxPrecedence = max max1 max2
        }
      where
        dir' = case (dir1, dir2) of
          (Just a, Just b) | a == b -> Just a
          _ -> Nothing

-- | The lowest level of information we can have about an operator.
defaultFixityApproximation :: FixityApproximation
defaultFixityApproximation = fixityInfoToApproximation defaultFixityInfo

-- | Convert from 'FixityInfo' to 'FixityApproximation'.
fixityInfoToApproximation :: FixityInfo -> FixityApproximation
fixityInfoToApproximation FixityInfo {..} =
  FixityApproximation
    { faDirection = Just fiDirection,
      faMinPrecedence = fiPrecedence,
      faMaxPrecedence = fiPrecedence
    }

-- | The map of operators declared by each package grouped by module name.
newtype HackageInfo
  = HackageInfo (Map PackageName (Map ModuleName (Map OpName FixityInfo)))
  deriving stock (Generic)
  deriving anyclass (Binary)

-- | Map from the operator name to its 'FixityInfo'.
newtype FixityOverrides = FixityOverrides
  { unFixityOverrides :: Map OpName FixityInfo
  }
  deriving stock (Eq, Show)

-- | Fixity overrides to use by default.
defaultFixityOverrides :: FixityOverrides
defaultFixityOverrides = FixityOverrides Map.empty

-- | Module re-exports
newtype ModuleReexports = ModuleReexports
  { unModuleReexports :: Map ModuleName (NonEmpty (Maybe PackageName, ModuleName))
  }
  deriving stock (Eq, Show)

-- | Module re-exports to apply by default.
defaultModuleReexports :: ModuleReexports
defaultModuleReexports =
  ModuleReexports . Map.fromList $
    [ ( "Control.Lens",
        l
          "lens"
          [ "Control.Lens.At",
            "Control.Lens.Cons",
            "Control.Lens.Each",
            "Control.Lens.Empty",
            "Control.Lens.Equality",
            "Control.Lens.Fold",
            "Control.Lens.Getter",
            "Control.Lens.Indexed",
            "Control.Lens.Iso",
            "Control.Lens.Lens",
            "Control.Lens.Level",
            "Control.Lens.Plated",
            "Control.Lens.Prism",
            "Control.Lens.Reified",
            "Control.Lens.Review",
            "Control.Lens.Setter",
            "Control.Lens.TH",
            "Control.Lens.Traversal",
            "Control.Lens.Tuple",
            "Control.Lens.Type",
            "Control.Lens.Wrapped",
            "Control.Lens.Zoom"
          ]
      ),
      ( "Servant",
        l
          "servant"
          [ "Servant.API"
          ]
      ),
      ( "Optics",
        l
          "optics"
          [ "Optics.Fold",
            "Optics.Operators",
            "Optics.IxAffineFold",
            "Optics.IxFold",
            "Optics.IxTraversal",
            "Optics.Traversal"
          ]
      ),
      ( "Test.Hspec",
        l
          "hspec-expectations"
          [ "Test.Hspec.Expectations"
          ]
      )
    ]
  where
    l packageName xs = (Just packageName,) <$> NE.fromList xs

-- | Fixity information that is specific to a package being formatted. It
-- requires module-specific imports in order to be usable.
newtype PackageFixityMap
  = PackageFixityMap (Map OpName (NonEmpty (PackageName, ModuleName, FixityInfo)))
  deriving stock (Eq, Show)

-- | Fixity map that takes into account imports in a particular module.
newtype ModuleFixityMap
  = ModuleFixityMap (Map OpName FixityProvenance)
  deriving stock (Eq, Show)

-- | Provenance of fixity info.
data FixityProvenance
  = -- | 'FixityInfo' of a built-in operator or provided by a user override.
    Given FixityInfo
  | -- | 'FixityInfo' to be inferred from module imports.
    FromModuleImports (NonEmpty (FixityQualification, FixityInfo))
  deriving stock (Eq, Show)

-- | Fixity qualification that determines how 'FixityInfo' matches a
-- particular use of an operator, given whether it is qualified or
-- unqualified and the module name used.
data FixityQualification
  = UnqualifiedAndQualified ModuleName
  | OnlyQualified ModuleName
  deriving stock (Eq, Show)

-- | Get a 'FixityApproximation' of an operator.
inferFixity ::
  -- | Whether to print debug info regarding fixity inference
  Choice "debug" ->
  -- | Operator name
  RdrName ->
  -- | Module fixity map
  ModuleFixityMap ->
  -- | The resulting fixity approximation
  FixityApproximation
inferFixity debug rdrName (ModuleFixityMap m) =
  if Choice.isTrue debug
    then
      trace
        (renderFixityJustification opName moduleName m result)
        result
    else result
  where
    result =
      case Map.lookup opName m of
        Nothing -> defaultFixityApproximation
        Just (Given fixityInfo) ->
          fixityInfoToApproximation fixityInfo
        Just (FromModuleImports xs) ->
          let isMatching (provenance, _fixityInfo) =
                case provenance of
                  UnqualifiedAndQualified mn ->
                    maybe True (== mn) moduleName
                  OnlyQualified mn ->
                    maybe False (== mn) moduleName
           in fromMaybe defaultFixityApproximation
                . foldMap (Just . fixityInfoToApproximation . snd)
                $ NE.filter isMatching xs
    opName = occOpName (rdrNameOcc rdrName)
    moduleName = case rdrName of
      Qual x _ -> Just (ghcModuleNameToCabal x)
      _ -> Nothing

-- | Render a human-readable account of why a certain 'FixityApproximation'
-- was chosen for an operator.
renderFixityJustification ::
  -- | Operator name
  OpName ->
  -- | Qualification of the operator name
  Maybe ModuleName ->
  -- | Module fixity map
  Map OpName FixityProvenance ->
  -- | The chosen fixity approximation
  FixityApproximation ->
  String
renderFixityJustification opName mqualification m approximation =
  concat
    [ "FIXITY analysis of ",
      show opName,
      case mqualification of
        Nothing -> ""
        Just mn -> " qualified in " ++ show mn,
      "\n  Provenance: " ++ show (Map.lookup opName m),
      "\n  Inferred: " ++ show approximation
    ]
