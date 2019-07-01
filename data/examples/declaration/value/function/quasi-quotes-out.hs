{-# LANGUAGE QuasiQuotes #-}

singleline :: Value
singleline = [yamlQQ|something|]

multiline :: Value
multiline =
  [yamlQQ| name: John Doe
age: 23

something: foo
|]
