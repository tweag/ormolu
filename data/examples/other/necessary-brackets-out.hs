insertEmDash = Text.concat . map \case { x | x == "--" -> "—"; x -> x } . Text.groupBy ((==) `on` Char.isSpace)

foo f a b c = f do { a } + f do { b } + f do c
