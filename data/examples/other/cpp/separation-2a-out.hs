ffff, ffffffff :: Natural

#ifdef HASKELL_ZIP_DEV_MODE
ffff     = 200
ffffffff = 5000
#else
ffff     = 0xffff
ffffffff = 0xffffffff
#endif
