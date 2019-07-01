{-# LANGUAGE TemplateHaskell #-}

bar = $bar

bar' = $(bar "something")

baz = $$baz

baz' = $$(baz "something")
