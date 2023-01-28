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
          { platformArchOS =
              ArchOS
                { archOS_arch = ArchUnknown,
                  archOS_OS = OSUnknown
                },
            platformWordSize = PW8,
            platformUnregisterised = True,
            platformByteOrder = LittleEndian,
            platformHasGnuNonexecStack = False,
            platformHasIdentDirective = False,
            platformHasSubsectionsViaSymbols = False,
            platformIsCrossCompiling = False,
            platformLeadingUnderscore = False,
            platformTablesNextToCode = False,
            platformHasLibm = False,
            platform_constants = Nothing
          },
      sPlatformMisc = PlatformMisc {},
      sToolSettings =
        ToolSettings
          { toolSettings_opt_P_fingerprint = fingerprint0,
            toolSettings_pgm_F = ""
          }
    }

baseDynFlags :: DynFlags
baseDynFlags = defaultDynFlags fakeSettings
