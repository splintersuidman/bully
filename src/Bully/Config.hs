module Bully.Config (
  ServerConfig (..),
  defaultServerConfig,
) where

import Network.Wai.Handler.Warp (Port)

newtype ServerConfig = ServerConfig
  { apiPort :: Port
  -- ^ Port on which the server should run.
  }
  deriving stock (Show, Eq)

defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { apiPort = 8080
    }
