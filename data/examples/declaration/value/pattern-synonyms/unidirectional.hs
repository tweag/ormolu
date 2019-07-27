{-# LANGUAGE PatternSynonyms #-}

pattern Head x <- x:xs

pattern Head' x
  <- x:xs

pattern Simple <- "Simple"

pattern WithTypeSig :: String
pattern WithTypeSig <- "WithTypeSig"
