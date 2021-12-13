type PermuteRef =
  "a"
    :> ( "b" :> "c" :> End
           :<|> "c" :> "b" :> End
       )
    :<|> "b"
      :> ( "a" :> "c" :> End
             :<|> "c" :> "a" :> End
         )
