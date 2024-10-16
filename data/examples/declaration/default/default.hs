module MyModule (default Monoid) where

default        (  Int , Foo     , Bar      )

default ( Int
               , Foo,
  Bar
           )

default Num (Int, Float)
default IsList ([], Vector)

default IsString (Text.Text, Foundation.String, String)
