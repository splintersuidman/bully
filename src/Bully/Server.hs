{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bully.Server (serve) where

import Bully.Compiler (runCompiler)
import Bully.Compiler qualified as Compiler
import Bully.Config (ServerConfig (..))
import Bully.Types (Bulletin)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Servant (Get, JSON, NamedRoutes, OctetStream, ReqBody, (:-), (:>))
import Servant qualified

serve :: ServerConfig -> IO ()
serve config = do
  Text.putStrLn $ "Starting server on port " <> Text.show config.apiPort
  Warp.run config.apiPort app

type Api = NamedRoutes Routes

type CompileReqBody = Bulletin Text ByteString

newtype Routes hkd = Routes
  { compile :: hkd :- "compile" :> ReqBody '[JSON] CompileReqBody :> Get '[OctetStream] ByteString
  }
  deriving stock (Generic)

app :: Application
app =
  Servant.serve @Api Proxy $
    Routes
      { compile = liftIO . runCompiler . Compiler.compile
      }
