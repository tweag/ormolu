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

The following examples show how a program is formatted in
multiple lines when the user introduces linebreaks. Otherwise,
the programs would be written in one line.

### Function types

```Haskell
functionName
  :: (C1, C2, C3)
  => a
  -> b
  -> c
  -> d
  -> (a, b, c, d)
```

Context
```Haskell
functionName
  :: ( C1
     , C2
     , C3
     , C4
     , C5
     )
  => a
  -> b
  -> c
  -> d
  -> (a, b, c, d)
```

Arguments
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

```Haskell
f x y z =
    if x then longFunctionName y else anotherLongFunctionName z
```

A lambda
```Haskell
f =
    \(LongPattern x) ->
    longFunctionName x
```

Multiple lambdas
```Haskell
f =
    \(LongPattern x) ->
    \(LongPattern2 y) ->
    longFunctionName x
```

Lambda arguments
```Haskell
f xs =
    forM xs $
    \( LongPattern x
     , LongPattern2 y
     ) ->
    longFunctionName x
```

There is a do block. Note the change of indentation in the
body of `forM`.
```Haskell
f xs = do
    forM xs $
      \( LongPattern x
       , LongPattern2 y
       ) ->
      longFunctionName x
```

`if` expression
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

Branches of `case` expression
```Haskell
f x y z = case
    Just w ->
      longFunctionName x y w
    Nothing ->
      anotherLongFunctionName x y z
```

A list of arguments
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

The right-hand side of guards
```Haskell
f x y
  | x > y =
    longFunctionName y
  | otherwise =
    anotherLongFunctionName x
```

Long guards
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

### Data type declarations

Declaration
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

### Comments

```Haskell
{- When a comment is not preceded with elements on the same line,
   they are indented as the elements in the enclosing AST node or
   braces/parenthesis/brackets.
   If it is between AST nodes, it is indented as the following
   node.
   If the end-of-file follows, then indent to column 0.
-}

f = g {- When there are preceding elements, keep single-line comments in the same line. -}

h = g
{- Multi-line comments can't be kept on the same line as @h = g@.
   Otherwise, changing h or g would change the indentation of all
   the comment lines.
 -}

data R = R
    {- This comment always follows the R constructor -}
    { f1 :: T1
      {- A comment not preceded by other elements -}
      {- Another comment -}
    , f2 :: T2
    , f3 :: T3
    }
```

```Haskell
{- a comment line -}
f = h
  where
    h = g
    -- indented the same as g
    g = False
```

This example
```Haskell
data R = R {- This comment always follows the R constructor -}
  { f1 :: T1, {- An inner comment -} {- Another inner comment -} f2 :: T2, f3 :: T3}
```
is reformatted to
```Haskell
data R = R
    {- This comment always follows the R constructor -}
    { f1 :: T1 {- An inner comment -} {- Another inner comment -}
    , f2 :: T2
    , f3 :: T3
    }
```
