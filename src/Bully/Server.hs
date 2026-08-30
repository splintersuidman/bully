{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bully.Server (serve) where

import Bully.Compiler (ContentDisposition (..), runCompiler)
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
import Servant (Header, Headers, JSON, NamedRoutes, OctetStream, Post, Raw, ReqBody, addHeader, (:-), (:>))
import Servant qualified

serve :: ServerConfig -> IO ()
serve config = do
  Text.putStrLn $ "Starting server on port " <> Text.show config.apiPort
  Warp.run config.apiPort app

type Api = NamedRoutes Routes

type CompileReqBody = Bulletin Text ByteString

data Routes hkd = Routes
  { compile :: hkd :- "compile" :> ReqBody '[JSON] CompileReqBody :> Post '[OctetStream] (Headers '[Header "Content-Disposition" Text] ByteString)
  , static :: hkd :- "static" :> Raw
  }
  deriving stock (Generic)

app :: Application
app =
  Servant.serve @Api Proxy $
    Routes
      { compile = \bulletin -> do
          (ContentDisposition contentDisposition, result) <- liftIO $ runCompiler $ Compiler.compile bulletin
          pure $ addHeader @"Content-Disposition" contentDisposition result
      , static = Servant.serveDirectoryWebApp "static"
      }
