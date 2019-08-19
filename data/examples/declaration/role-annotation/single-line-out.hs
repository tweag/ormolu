{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}

type role Ptr representational

type role A nominal nominal

type role B _ phantom

type role C _ _
