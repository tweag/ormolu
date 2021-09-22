{-# LANGUAGE QuasiQuotes #-}

x = [foo|    foo bar   |]

x = [e|    foo
 bar  {- -}
  |]

[d|    foo bar

|]

header = [here|
#include foo|]
