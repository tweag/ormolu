mergeErrorReply :: ParseError -> Reply s u a -> Reply s u a
mergeErrorReply err1 reply -- XXX where to put it?
  =
  case reply of
    Ok x state err2 -> Ok x state (mergeError err1 err2)
    Error err2 -> Error (mergeError err1 err2)
