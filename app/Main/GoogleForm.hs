{-# LANGUAGE OverloadedStrings #-}

module Main where

import Bully
import Bully.Toml
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as BL
import Data.Csv ((.:))
import Data.Csv qualified as Csv
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Time qualified as Time
import Data.Vector (Vector)
import Data.Vector qualified as Vector
import System.Directory (setCurrentDirectory)
import System.Environment (getArgs)
import System.FilePath (takeDirectory)
import Text.Pandoc.UTF8 qualified as Utf8
import Toml (TomlCodec, (.=))
import Toml qualified

data Form = Form
  { formAuthorLabel :: !ByteString
  , formTitleLabel :: !ByteString
  , formDateLabel :: !ByteString
  , formSourceLabel :: !ByteString
  }
  deriving (Show, Eq)

formCodec :: TomlCodec Form
formCodec =
  Form
    <$> Toml.byteString "author" .= formAuthorLabel
    <*> Toml.byteString "title" .= formTitleLabel
    <*> Toml.byteString "date" .= formDateLabel
    <*> Toml.byteString "source" .= formSourceLabel

newtype GoogleSheetsDay = GoogleSheetsDay Time.Day

instance Csv.FromField GoogleSheetsDay where
  parseField = fmap GoogleSheetsDay . Time.parseTimeM True Time.defaultTimeLocale "%-d-%-m-%Y %H:%M:%S" . Utf8.toString

newtype GoogleSheetsSource = GoogleSheetsSource Source

instance Csv.FromField GoogleSheetsSource where
  parseField s = case BS8.split '=' s of
    ["https://drive.google.com/open?id", docId] -> pure $ GoogleSheetsSource $ SourceGoogleDrive $ Text.decodeUtf8 docId
    _ -> fail $ "no parse of Google Sheets link " <> show s

formParser :: Form -> Csv.NamedRecord -> Csv.Parser (Contribution Input)
formParser form record = do
  author <- record .: formAuthorLabel form
  title <- record .: formTitleLabel form
  GoogleSheetsDay date <- record .: formDateLabel form
  GoogleSheetsSource source <- record .: formSourceLabel form
  pure $
    Contribution
      { contributionAuthor = author
      , contributionTitle = title
      , contributionDate = date
      , contributionDocument = Input {inputSource = source, inputFormat = Nothing}
      }

decodeForm :: Form -> BL.ByteString -> Either String (Vector (Contribution Input))
decodeForm form = fmap snd . Csv.decodeByNameWithP (formParser form) Csv.defaultDecodeOptions

newtype GoogleSheet = GoogleSheet Text
  deriving (Show, Eq)

googleSheetCodec :: Toml.Key -> TomlCodec GoogleSheet
googleSheetCodec = Toml.dimap (\(GoogleSheet sheetId) -> sheetId) GoogleSheet . Toml.text

readGoogleSheet :: GoogleSheet -> Bully BL.ByteString
readGoogleSheet (GoogleSheet sheetId) = readUrl $ "https://docs.google.com/spreadsheets/d/" <> sheetId <> "/export?format=csv"

readGoogleSheetContributions :: Form -> GoogleSheet -> Bully [Contribution Input]
readGoogleSheetContributions form sheet = do
  csv <- readGoogleSheet sheet
  contributions <- liftEither' BulletinCsvParseError $ decodeForm form csv
  pure $ Vector.toList contributions

data Config = Config
  { configBulletin :: !(Bulletin FilePath Input)
  , configForm :: !Form
  , configSheet :: !GoogleSheet
  }
  deriving (Show)

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> bulletinCodec .= configBulletin
    <*> Toml.table formCodec "form" .= configForm
    <*> googleSheetCodec "sheet" .= configSheet

readConfig :: FilePath -> Bully Config
readConfig filename = do
  res <- Toml.decodeFileEither configCodec filename
  liftEither' BulletinTomlDecodeError res

readBulletinConfig :: Config -> Bully (Bulletin FilePath Input)
readBulletinConfig config = do
  contributions <- readGoogleSheetContributions (configForm config) (configSheet config)
  let bulletin = configBulletin config
  pure $
    bulletin
      { bulletinContributions = bulletinContributions bulletin <> contributions
      }

main :: IO ()
main = runBully $ do
  args <- liftIO getArgs
  when (null args) $
    throwError $
      BulletinUsageError "Specify configuration file(s): bulletin-google-form <file ...>"

  for_ args $ \configFile -> do
    config <- readConfig configFile
    bulletinConfig <- readBulletinConfig config
    -- Set directory to that of config file, to deal with paths relative to it.
    liftIO $ setCurrentDirectory $ takeDirectory configFile
    bulletin <- readTemplates . processContributions =<< readContributions bulletinConfig
    writeOutputs bulletin $ compileBulletin bulletin
