{-# LANGUAGE CApiFFI #-}

foreign import ccall safe foo :: Int -> IO Int

-- | 'bar' is a very important thing
foreign import stdcall "baz" bar :: String -> Int -> IO String

foreign import stdcall unsafe "boo"
  -- Here is a comment about my foreign function
  boo :: Int -> Text -> IO Array

foreign import javascript
  baz
    :: String
    -> Int
    -> IO Foo

foreign import {- We use capi here -} capi "pi.h value pi" c_pi :: CDouble

foreign import stdcall {- This is a bad place for a comment -} "dynamic"
  dyn_gluBeginSurface
    :: FunPtr (Ptr GLUnurbs -> IO ()) -- ^ This 'FunPtr' is extremely dangerous, beware
    -> Ptr GLUnurbs
    -> IO ()
