withGuards :: Int -> Int
withGuards x =
  case x of
    x
      | x > 10 ->
        foo
          + bar
    x | x > 5 -> 10
    _ -> 20

case x of
  '-' | not isUrl -> case xs of
    _ -> emitc '-'
  '*' | not isUrl ->
    case xs of
      _ -> emitc '*'
