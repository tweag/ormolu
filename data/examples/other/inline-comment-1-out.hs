showPs env ((n, _, Let _ t v) : bs) =
  "  "
    ++ show n
    ++ " : "
    ++ showEnv env ({- normalise ctxt env -} t)
    ++ "   =   "
    ++ showEnv env ({- normalise ctxt env -} v)
    ++ "\n"
    ++ showPs env bs
showPs env ((n, _, b) : bs) =
  "  "
    ++ show n
    ++ " : "
    ++ showEnv env ({- normalise ctxt env -} (binderTy b))
    ++ "\n"
    ++ showPs env bs
