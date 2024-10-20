{-# LANGUAGE MultilineStrings #-}

type Foo = """
  yeah
    yeah"""

foo = foo @"""yeah
           yeah
           """
