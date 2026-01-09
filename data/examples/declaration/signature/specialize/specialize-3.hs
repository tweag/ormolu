sep, fsep, hsep :: (Applicative m, Foldable t) => t (m Doc) -> m Doc
sep  = fmap P.sep  . sequenceAFoldable  ; {-# SPECIALIZE NOINLINE sep  :: [TCM Doc] -> TCM Doc #-} ; {-# SPECIALIZE NOINLINE sep  :: List1 (TCM Doc) -> TCM Doc #-}
fsep = fmap P.fsep . sequenceAFoldable  ; {-# SPECIALIZE NOINLINE [2] fsep :: [TCM Doc] -> TCM Doc #-} ; {-# SPECIALIZE NOINLINE [2] fsep :: List1 (TCM Doc) -> TCM Doc #-}
hsep = fmap P.hsep . sequenceAFoldable  ; {-# SPECIALIZE NOINLINE [~2] hsep :: [TCM Doc] -> TCM Doc #-} ; {-# SPECIALIZE NOINLINE [~2] hsep :: List1 (TCM Doc) -> TCM Doc #-}
