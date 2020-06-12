magnify ::
  forall outertag innertag t outer inner m a.
  ( forall x. Coercible (t m x) (m x),
    HasReader outertag outer m,
    forall m'.
    HasReader outertag outer m' =>
    HasReader innertag inner (t m')
  ) =>
  (forall m'. HasReader innertag inner m' => m' a) ->
  m a
