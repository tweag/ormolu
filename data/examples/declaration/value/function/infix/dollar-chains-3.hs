ex1 =
  f1 $
    arg1 $
      arg2 $ arg3

ex2 =
  f1 $
    arg1 $
      arg2 $ f2 arg3

ex3 =
  f1 $
    arg1 $
      arg2 $ 1 + 3

ex4 =
  f1 $
    arg1 $
      arg2 $ do
        arg3

ex5 =
  f1 $
    arg1 $
      arg2 $ \c ->
        3 * c

ex6 =
  f1 $
    arg1 $
      arg2 $ do arg3

ex7 =
  f1 $
    arg1 $
      arg2 $ \c -> 3 * c
