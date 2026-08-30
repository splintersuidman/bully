{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Bully
import           Bully.Toml
import           Control.Monad          (when)
import           Control.Monad.Except   (MonadError (throwError))
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Foldable          (for_)
import           System.Directory       (setCurrentDirectory)
import           System.Environment     (getArgs)
import           System.FilePath        (takeDirectory)
import qualified Toml

-- | Read the bulletin configuration from the given file.
readBulletinConfig :: FilePath -> Bully (Bulletin FilePath Input)
readBulletinConfig filename = do
  res <- Toml.decodeFileEither bulletinCodec filename
  liftEither' BulletinTomlDecodeError res

main :: IO ()
main = runBully $ do
  args <- liftIO getArgs
  when (null args) $
    throwError $ BulletinUsageError "Specify configuration file(s): bulletin <file ...>"

  for_ args $ \configFile -> do
    bulletinConfig <- readBulletinConfig configFile
    -- Set directory to that of config file, to deal with paths relative to it.
    liftIO $ setCurrentDirectory $ takeDirectory configFile
    bulletin <- readTemplates . processContributions =<< readContributions bulletinConfig
    writeOutputs bulletin $ compileBulletin bulletin
