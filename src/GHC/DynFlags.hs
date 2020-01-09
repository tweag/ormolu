{-# OPTIONS_GHC -Wno-missing-fields #-}

module GHC.DynFlags
  ( baseDynFlags,
  )
where

import Config
import DynFlags
import Fingerprint
import Platform

-- | Taken from HLint.
fakeSettings :: Settings
fakeSettings =
  Settings
    { sTargetPlatform = platform,
      sPlatformConstants = platformConstants,
      sProjectVersion = cProjectVersion,
      sProgramName = "ghc",
      sOpt_P_fingerprint = fingerprint0,
      sPgm_F = ""
    }
  where
    platform =
      Platform
        { platformWordSize = 8,
          platformOS = OSUnknown,
          platformUnregisterised = True
        }
    platformConstants =
      PlatformConstants {pc_DYNAMIC_BY_DEFAULT = False, pc_WORD_SIZE = 8}

fakeLlvmConfig :: (LlvmTargets, LlvmPasses)
fakeLlvmConfig = ([], [])

baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings fakeLlvmConfig
