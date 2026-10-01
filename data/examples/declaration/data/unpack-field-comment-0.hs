data Buffer = Buffer {-# UNPACK #-} !(ForeignPtr Word8) -- underlying pinned array
                     {-# UNPACK #-} !(Ptr Word8)        -- beginning of slice
