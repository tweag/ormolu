deriving instance Eq Foo

deriving stock instance Show Foo

deriving anyclass instance ToJSON Foo

deriving newtype instance Data Foo

deriving instance {-# OVERLAPPABLE #-} Ord Foo

deriving instance {-# OVERLAPPING #-} Num Foo

deriving instance {-# OVERLAPS #-} Read Foo

deriving instance {-# INCOHERENT #-} Show Foo

deriving via Int instance Triple A B C
