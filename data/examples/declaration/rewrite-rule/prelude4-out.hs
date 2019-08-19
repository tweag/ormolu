{-# RULES
"unpack" [~1] forall a. unpackCString # a = build (unpackFoldrCString # a)
"unpack-list" [1] forall a. unpackFoldrCString # a (:) [] = unpackCString # a
"unpack-append" forall a n. unpackFoldrCString # a (:) n = unpackAppendCString # a n
  #-}
-- There's a built-in rule (in PrelRules.lhs) for
--      unpackFoldr "foo" c (unpackFoldr "baz" c n)  =  unpackFoldr "foobaz" c n
