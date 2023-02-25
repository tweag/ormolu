import qualified Database.Esqueleto.Experimental as E

foo =
  E.from $
    E.table
      `E.innerJoin` E.table
      `E.on` ( \(a :& b) ->
               a E.^. AField E.==. b E.^. BField
             )
