instance DemoteNodeTypes ('[] :: [NodeType]) where
    demoteNodeTypes _ = []

b :: (Bool :: *)
b = True

unP :: forall {k} (a :: k). P a -> Proxy a

data ProxyKInvis (a :: k) :: forall k. k -> Type
data ProxyKVis k (a :: k) :: forall k -> k -> Type
