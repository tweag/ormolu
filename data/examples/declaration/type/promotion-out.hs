type Foo = Cluster '[ 'NodeCore, 'NodeRelay', 'NodeEdge]

data T = T' | T'T

type S0 = ' T'

type S1 = ' T'T

type S2 = Proxy ('[ '[], '[]])

type S4 = Proxy ('( 'Just, ' T'T))

type S5 = Proxy ('[ 'Just, 'TT'T])

type S6 = Proxy ('( '(), '()))

type S7 = Proxy ('( 'a, 'b))

type S8 = Proxy ('[Int, Bool])
