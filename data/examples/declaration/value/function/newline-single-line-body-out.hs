function :: Int -> Int
function a =
  aReallyLongFunctionNameThatShouldStayOnThisLineToAvoidOverflowing 10000 a

function' :: String -> String
function' s = case s of
  "ThisString" ->
    -- And a comment here is okay
    "Yay"
  _ -> "Boo"
