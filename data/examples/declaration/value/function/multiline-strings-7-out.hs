{-# LANGUAGE MultilineStrings #-}

printf
  """
  instance Aeson.FromJSON %s where
    parseJSON =
      Aeson.withText "%s" $ \\s ->
        either Aeson.parseFail pure $
          parsePrinterOptType (Text.unpack s)

  instance PrinterOptsFieldType %s where
    parsePrinterOptType s =
      case s of
  %s
        _ ->
          Left . unlines $
            [ "unknown value: " <> show s
            , "Valid values are: %s"
            ]

  """
  fieldTypeName
  fieldTypeName
  fieldTypeName
  ( unlines_
      [ printf "      \"%s\" -> Right %s" val con
      | (con, val) <- enumOptions
      ]
  )
  (renderEnumOptions enumOptions)
