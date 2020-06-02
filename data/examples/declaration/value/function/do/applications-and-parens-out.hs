warningFor var place = do
  guard $ isVariableName var
  guard . not $ isInArray var place || isGuarded place
  (if includeGlobals || isLocal var
     then warningForLocals
     else warningForGlobals)
    var
    place
