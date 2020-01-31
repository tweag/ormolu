( getNodeSettingsR
    :<|> getNodeInfoR
    :<|> getNextUpdateR
    :<|> restartNodeR
  )
  :<|> ( getUtxoR
           :<|> getConfirmedProposalsR
         ) = client nodeV1Api
