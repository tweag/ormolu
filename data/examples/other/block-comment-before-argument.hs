checkPragma =
        ifM (anyM isBuiltin [builtinNat, builtinBool])
          {-then-} ok
          {-else-} notPostulate
