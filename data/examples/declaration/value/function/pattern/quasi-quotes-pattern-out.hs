{-# LANGUAGE QuasiQuotes #-}

singleline :: ()
singleline [yamlQQ|something|] = ()

multiline :: ()
multiline = case y of
  [yamlQQ| name: John Doe
age: 23
|] -> ()
