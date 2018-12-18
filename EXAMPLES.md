We aim for a format of Haskell code that minimizes the amount of changes
necessary when modifying a line. For instance the indentation of any
given fragment of code should not depend on the length of identifiers.

```Haskell
longFunctionName :: a
                 -> b
                 -> c
                 -> d
                 -> (a, b, c, d)
```

Renaming the function requires changing all the lines to 
```Haskell
f :: a
  -> b
  -> c
  -> d
  -> (a, b, c, d)
```

A preferred alternative is to format like
```Haskell
longFunctionName
  :: a
  -> b
  -> c
  -> d
  -> (a, b, c, d)
```

We are grabbing some inspiration from
[Elm](https://elm-lang.org/docs/style-guide)
and
[Tweag's](https://github.com/tweag/guides/blob/master/style/Haskell.md)
style guides.

### Function types

Type and name fits in one line
```Haskell
functionName :: (C1, C2) => a -> b -> (a, b)
```

Type and name don't fit in one line.
```Haskell
functionName
  :: (C1, C2, C3) => a -> b -> c -> d -> (a, b, c, d)
```

Context, arguments and result don't fit in one line.
```Haskell
functionName
  :: (C1, C2, C3, C4, C5)
  => a -> b -> c -> d -> (a, b, c, d)
```

Arguments and result don't fit in one line.
```Haskell
functionName
  :: (C1, C2, C3, C4, C5)
  => a
  -> b
  -> (a -> b -> c)
  -> (c -> d)
  -> (a, b, c, d)
```

Arguments don't fit in one line.
```Haskell
functionName
  :: (C1, C2, C3, C4, C5)
  => a
  -> b
  -> (forall a.
        (C6, C7)
     => LongDataTypeName
     -> a
     -> AnotherLongDataTypeName
     -> b 
     -> c
     )
  -> (c -> d)
  -> (a, b, c, d)
```

Arguments don't fit in one line.
```Haskell
functionName
  :: (C1, C2, C3, C4, C5)
  => a
  -> b
  -> (  LongDataTypeName
          AnotherLongDataTypeName
          AnotherLongDataTypeName2
          AnotherLongDataTypeName3
     -> a
     -> AnotherLongDataTypeName4
     -> b
     -> c
     )
  -> (c -> d)
  -> (a, b, c, d)
```

### Expressions

Fits a line.
```Haskell
f x y z = if x then y else z
```

Doesn't fit a line.
```Haskell
f x y z =
    if x then longFunctionName y else anotherLongFunctionName z
```

A lambda fits a line.
```Haskell
f = \x -> x
```

A lambda doesn't fit the line.
```Haskell
f =
  \(LongPattern x) ->
  longFunctionName x
```

Multiple lambdas don't fit the line.
```Haskell
f = \(LongPattern x) ->
      \(LongPattern2 y) ->
        longFunctionName x
```

The lambda arguments don't fit the line.
```Haskell
f xs =
    forM xs $ \( LongPattern x
               , LongPattern2 y
               ) ->
      longFunctionName x
```

The lambda arguments and the preceding expression don't fit the line.
```Haskell
f xs =
  forM xs $
    \( LongPattern x
     , LongPattern2 y
     , LongPattern3 z
     ) ->
    longFunctionName x y z
```

`if` expression doesn't fit a line.
```Haskell
f x y z =
    if x then
      longFunctionName x y
    else
      anotherLongFunctionName x z
```

`if` expression with monadic blocks
```Haskell
f x y z =
    if x then do
      longFunctionName1 x y
      longFunctionName2 x y
    else do
      anotherLongFunctionName1 x z
      anotherLongFunctionName2 x z
```

Monadic blocks
```Haskell
f x y z = do
    functionName1 x y
    functionName2 x z
```

`case` expression
```Haskell
f x y z = case
    Just w -> functionName x y w
    Nothing -> functionName x y z
```

Branches of `case` expression don't fit a line.
```Haskell
f x y z = case
    Just w ->
      longFunctionName x y w
    Nothing ->
      anotherLongFunctionName x y z
```

A list of arguments doesn't fit a line.
```Haskell
f
    (LongPattern x)
    (AnotherLongPattern y)
    =
    functionName x y
```

Guards
```Haskell
f x y
  | x > y = True
  | otherwise = False
```

The right-hand side of guards doesn't fit in a line.
```Haskell
f x y
  | x > y =
    longFunctionName y
  | otherwise =
    anotherLongFunctionName x
```

Long guards don't fit the line.
```Haskell
function x y z w y
  | x > y
  , y > z
  , z > w
  , w > z
  =
    longFunctionName y
  | otherwise =
    anotherLongFunctionName x
```

Long guards with monadic block
```Haskell
function x y z w y
  | x > y
  , y > z
  , z > w
  , w > z
  = do
    longFunctionName1 y
    longFunctionName2 y
  | otherwise = do
    anotherLongFunctionName1 x
    anotherLongFunctionName2 x
```

### Data type declarations

Declaration fits in one line.
```Haskell
data A = B | C | D
```

Declaration doesn't fit in one line.
```Haskell
data A
  = B
  | C
  | D
  | E
  | F
  | G
```

TODO: GADTs, record syntax, type families
