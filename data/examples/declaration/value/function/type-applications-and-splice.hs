{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

staticKey name = [| sing @ $(symFQN name) |]
