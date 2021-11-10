module Main where

foo =
  fmap escapeLeadingDollar
    . dropPaddingSpace
    . dropWhileEnd T.null
    . fmap (T.stripEnd . T.pack)
    . lines
    $ unpackHDS docStr

foo =
  when (GHC.xopt Cpp dynFlags && not cfgTolerateCpp) $
    throwIO (OrmoluCppEnabled path)

foo =
  bar
    $ baz
    $ quux

x =
  case l of { A -> B } $
  case q of r -> s
