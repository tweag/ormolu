decompressingPipe
  :: (PrimMonad m, MonadThrow m, MonadResource m)
  => CompressionMethod
  -> ConduitT ByteString ByteString m ()
decompressingPipe Store = C.awaitForever C.yield
decompressingPipe Deflate = Z.decompress $ Z.WindowBits (-15)

#ifdef ENABLE_BZIP2
decompressingPipe BZip2   = BZ.bunzip2
#else
decompressingPipe BZip2   = throwM BZip2Unsupported
#endif

foo :: Int
foo = undefined
