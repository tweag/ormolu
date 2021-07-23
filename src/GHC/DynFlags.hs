{-# OPTIONS_GHC -Wno-missing-fields #-}

-- Modified from ghc-lib-api-ext.

module GHC.DynFlags
  ( baseDynFlags,
  )
where

import GHC.Driver.Session
import GHC.Platform
import GHC.Settings
import GHC.Settings.Config
import GHC.Utils.Fingerprint

fakeSettings :: Settings
fakeSettings =
  Settings
    { sGhcNameVersion =
        GhcNameVersion
          { ghcNameVersion_programName = "ghc",
            ghcNameVersion_projectVersion = cProjectVersion
          },
      sFileSettings = FileSettings {},
      sTargetPlatform =
        Platform
          { platformWordSize = PW8,
            platformMini =
              PlatformMini
                { platformMini_arch = ArchUnknown,
                  platformMini_os = OSUnknown
                },
            platformUnregisterised = True,
            platformByteOrder = LittleEndian,
            platformHasGnuNonexecStack = False,
            platformHasIdentDirective = False,
            platformHasSubsectionsViaSymbols = False,
            platformIsCrossCompiling = False,
            platformLeadingUnderscore = False,
            platformTablesNextToCode = False
          },
      sPlatformMisc = PlatformMisc {},
      sPlatformConstants =
        PlatformConstants {pc_DYNAMIC_BY_DEFAULT = False, pc_WORD_SIZE = 8},
      sToolSettings =
        ToolSettings
          { toolSettings_opt_P_fingerprint = fingerprint0,
            toolSettings_pgm_F = ""
          }
    }

fakeLlvmConfig :: LlvmConfig
fakeLlvmConfig = LlvmConfig [] []

baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings fakeLlvmConfig
