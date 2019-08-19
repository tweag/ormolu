{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE MagicHash #-}

type role Ptr representational

type role A nominal nominal

type role B _ phantom
type role C _ _
