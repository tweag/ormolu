scientifically :: (Scientific -> a) -> Parser a
scientifically h = do
  something
  (I.satisfy (\w -> w == 'e' || w == 'E')
     *> fmap (h . Sci.scientific signedCoeff . (e +)) (signed decimal))
    <|> return (h $ Sci.scientific signedCoeff e)
