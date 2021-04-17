{-# LANGUAGE TemplateHaskell #-}

$(deriveJSON fieldLabelMod ''A)
$(deriveJSON fieldLabelMod ''B)
$(deriveJSON fieldLabelMod ''C)
