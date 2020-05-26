main :: IO ()
main = do
  migrateSchema
    [ migration1
    , migration1
    , migration3
    -- When adding migrations here, don't forget to update
    -- 'schemaVersion' in Galley.Data
    ]
    `finally` Log.close
