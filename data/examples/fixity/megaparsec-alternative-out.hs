module MegaparsecExample where

import Text.Megaparsec

pValue =
  Object <$> parseObjectBody
    <|> Array <$> parseArrayBody
    <|> String <$> parseStringBody
