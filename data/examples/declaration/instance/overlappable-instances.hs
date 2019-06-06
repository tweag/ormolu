-- Test should have tested different whitespaces inside pragma. However, the
-- pragmas contain a "SourceText" which compares as equal only if the whitespace
-- inside the pragma is printed exactly identically (i.e. including newlines and
-- all.) In practice, removing this whitespace is desirable, so whitespace is
-- not varied to avoid spurious and difficult-to-avoid test case errors.

instance {-# OVERLAPPABLE #-} Eq Int where (==) _ _ = False

instance
    {-# OVERLAPPING #-} Ord Int where compare _ _ = GT

instance {-# OVERLAPS #-} Eq Double where
    (==) _ _ = False

instance
    {-# INCOHERENT #-} Ord Double where
    compare _ _ = GT
