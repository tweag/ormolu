foo = Foo {a = 3}

bar =
  Bar
    { abc = foo
    , def = Foo {a = 10}
    }

baz = Baz {}

sym = Foo {(+) = 3}

aLongVariableName =
  ALongRecordName
    { short = baz
    , aLongRecordFieldName =
        YetAnotherLongRecordName
          { yetAnotherLongRecordFieldName = "a long string"
          }
    , aLongRecordFieldName2 =
        Just
          YetAnotherLongRecordName
            { yetAnotherLongRecordFieldName = "a long string"
            , yetAnotherLongRecordFieldName =
                Just
                  "a long string"
            }
    , aLongRecordFieldName3 = do
        foo
        bar
    }
