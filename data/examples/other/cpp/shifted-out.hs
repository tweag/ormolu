sinkData h compression = do
  (uncompressedSize, crc32, compressedSize) <-
    case compression of
      Store ->
        withCompression
          dataSink
      Deflate ->
        withCompression $
          Z.compress 9 (Z.WindowBits (-15)) .| dataSink
#ifdef ENABLE_BZIP2
      BZip2   -> withCompression $
        BZ.bzip2 .| dataSink
#else
      BZip2   -> throwM BZip2Unsupported
#endif
  return
    DataDescriptor
      { ddCRC32 = fromIntegral crc32
      , ddCompressedSize = compressedSize
      , ddUncompressedSize = uncompressedSize
      }
