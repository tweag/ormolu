module A (a) where

a :: Int
a = do
  f
    . do
      g
    $ do
      1
        + 2
        * 3
    $ ( 6
          * 5
          + 4
      )

b =
  f a <> 1 + 2 <> do
    3

c =
  f b $ 1 + 2 $ do
    3
