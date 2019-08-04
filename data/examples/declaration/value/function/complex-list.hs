handleStuff =
  handle
    [ \ExceptionA ->
        something
    , \ExceptionB ->
        somethingElse
    ]

handleStuff =
  handle
    [ foo
        bar
    , baz
        qux
    ]
