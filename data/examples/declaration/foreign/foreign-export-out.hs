foreign export ccall foo :: Int -> IO Int

-- | 'foreignTomFun' is a very important thing
foreign export ccall "tomography"
  foreignTomFun
    :: StablePtr Storage {- Storage is bad -} -> TomForegin

foreign export {- We can't use capi here -} ccall "dynamic"
  export_nullaryMeth :: (IO HRESULT) -> IO (Ptr ())
