module Main (main) where

import Bully.Config qualified as Config
import Bully.Server qualified as Server

main :: IO ()
main = Server.serve Config.defaultServerConfig
