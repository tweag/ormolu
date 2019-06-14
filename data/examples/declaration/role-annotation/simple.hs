{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE MagicHash #-}

type role Ptr representational

type role A nominal nominal

type role B _ phantom
type role C _ _

type role
  D phantom nominal

type
  role
    E
      _
    nominal

type
    role
      E
        _
      nominal
    phantom
