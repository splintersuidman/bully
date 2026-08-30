module Main (main) where

import Bully.Config qualified as Config
import Bully.Server qualified as Server

-- TODO: Parse config from command line arguments.
main :: IO ()
main = Server.serve Config.defaultServerConfig
