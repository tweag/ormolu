data FutureSumType
  = {- | This is what the documentation for this type looks like.
    It is pretty awkward that the @--@s aren't aligned.
    -}
    SingleConstructor
    { someRecordField :: Bool,
      someOtherRecordField :: Int
    }

data FutureSumType1
  = -- | This is what the documentation for this type looks like.
    -- It is pretty awkward that the @--@s aren't aligned.
    SingleConstructor1
    { someRecordField :: Bool,
      someOtherRecordField :: Int
    }

data FutureSumType1
  = {- | This is what the documentation for this type looks like.
         It is pretty awkward that the @--@s aren't aligned.
     -}
    SingleConstructor1
      { someRecordField :: Bool,
        someOtherRecordField :: Int
      }
  | Foo
