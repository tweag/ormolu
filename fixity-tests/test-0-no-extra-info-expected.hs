instance A.ToJSON UpdateTable where
  toJSON a =
    A.object $
      "TableName"
        .= updateTableName a
        : "ProvisionedThroughput"
        .= updateProvisionedThroughput a
        : case updateGlobalSecondaryIndexUpdates a of
          [] -> []
          l -> ["GlobalSecondaryIndexUpdates" .= l]
