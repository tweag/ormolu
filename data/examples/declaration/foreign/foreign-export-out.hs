foreign export ccall foo :: Int -> IO Int

-- | 'foreignTomFun' is a very important thing
foreign export ccall "tomography"
  foreignTomFun :: StablePtr Storage -> TomForegin {- Storage is bad -}

foreign export {- We can't use capi here -} ccall "dynamic"
  export_nullaryMeth :: (IO HRESULT) -> IO (Ptr ())
