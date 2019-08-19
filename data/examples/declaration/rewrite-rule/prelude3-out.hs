{-# LANGUAGE MagicHash #-}

{-# RULES
"x# `eqChar#` x#" forall x#. x# `eqChar#` x# = True
"x# `neChar#` x#" forall x#. x# `neChar#` x# = False
"x# `gtChar#` x#" forall x#. x# `gtChar#` x# = False
"x# `geChar#` x#" forall x#. x# `geChar#` x# = True
"x# `leChar#` x#" forall x#. x# `leChar#` x# = True
"x# `ltChar#` x#" forall x#. x# `ltChar#` x# = False
  #-}
