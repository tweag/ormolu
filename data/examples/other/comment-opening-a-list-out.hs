module Hledger.Cli.Commands where

commandsList :: String -> [String] -> [String]
commandsList progversion othercmds =
  map (bold' . accent) _banner_smslant
    ++ [ -- XXX not showing bold, why ?
         -- Keep the following synced with:
         --  commands.m4
         "----------",
         progversion
       ]
