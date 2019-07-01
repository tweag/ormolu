{-# LANGUAGE QuasiQuotes #-}

x = [foo|    foo bar   |]

x =
  [e|    foo
 bar  {- -}
  |]

[d|    foo bar

|]
