data Something (n :: Nat) = Something

data Format (k :: *) (k' :: *) (k'' :: *)

type Parens1 = Proxy '(a :: A, b :: (B :: *))
type Parens2 = Proxy '((a :: A), (b :: B))

type family Foo a where
  Foo '(a :: Int, b :: Bool) = Int
  Foo '((a :: Int), (b :: Bool)) = Int
