wrapError ::
  forall outertag innertag t outer inner m a.
  ( forall x. Coercible (t m x) (m x),
    HasCatch outertag outer m,
    forall m'.
    HasCatch outertag outer m' =>
    HasCatch innertag inner (t m')
  ) =>
  (forall m'. HasCatch innertag inner m' => m' a) ->
  m a
