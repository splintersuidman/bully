{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bully.Types (
  Bulletin (..),
  Contribution (..),
  ContributionFormat (..),
  Output (..),
  OutputFormat (..),
  PdfCompiler (..),
) where

import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Base64.Types (Alphabet (StdPadded), Base64)
import Data.Base64.Types qualified as Base64
import Data.ByteString (ByteString)
import Data.ByteString.Base64 qualified as ByteString.Base64
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Encoding.Base64 qualified as Text.Base64
import Data.Time qualified as Time
import Data.Vector (Vector)
import GHC.Generics (Generic)
import Text.Pandoc.Builder qualified as Pandoc
import Witch (From (..), into)

data Bulletin temp doc = Bulletin
  { title :: !Text
  , date :: !Time.Day
  , extra :: !(Map Text Text)
  , contributions :: !(Vector (Contribution doc))
  , output :: !(Output temp)
  }
  deriving stock (Show, Eq, Functor, Generic)

data Contribution doc = Contribution
  { author :: !Text
  , title :: !Text
  , date :: !Time.Day
  , format :: !ContributionFormat
  , document :: !doc
  }
  deriving stock (Show, Eq, Functor, Generic)

newtype ContributionFormat = ContributionFormat Text
  deriving newtype (Show, Eq, Aeson.ToJSON, Aeson.FromJSON)

instance From ContributionFormat Text where
  from (ContributionFormat format) = format

instance From Text ContributionFormat where
  from = ContributionFormat

data Output temp = Output
  { format :: !OutputFormat
  , template :: !temp
  }
  deriving stock (Show, Eq, Functor, Generic)

data OutputFormat
  = -- | PDF output with a Pandoc PDF compiler.
    OutputFormatPdf !PdfCompiler
  | -- | Other output format understood by Pandoc.
    OutputFormatOther !Text
  deriving stock (Show, Eq, Generic)

newtype PdfCompiler = PdfCompiler Text
  deriving newtype (Show, Eq, IsString, Aeson.FromJSON, Aeson.ToJSON)

instance From PdfCompiler Text where
  from (PdfCompiler compiler) = compiler

instance From Text PdfCompiler where
  from = PdfCompiler

--------------------------------------------------------------------------------

-- | Base 64-encoded 'ByteString'. Use 'From' instances to write Aeson instances for types where
-- bytestrings are base 64-encoded.
newtype Base64ByteString = Base64ByteString (Base64 'StdPadded ByteString)
  deriving newtype (Show, Eq)

instance From ByteString Base64ByteString where
  from = Base64ByteString . ByteString.Base64.encodeBase64'

instance From Base64ByteString ByteString where
  from (Base64ByteString base64) = ByteString.Base64.decodeBase64 base64

-- | Base 64-encoded 'Text'. Use 'From' instances to write Aeson instances for types where
-- strings are base 64-encoded.
newtype Base64Text = Base64Text (Base64 'StdPadded Text)
  deriving newtype (Show, Eq)

instance From Text Base64Text where
  from = Base64Text . Text.Base64.encodeBase64

instance From Base64Text Text where
  from (Base64Text base64) = Text.Base64.decodeBase64 base64

--------------------------------------------------------------------------------

instance Pandoc.ToMetaValue a => Pandoc.ToMetaValue (Contribution a) where
  toMetaValue contribution =
    Pandoc.toMetaValue @(Map Text Pandoc.MetaValue) $
      Map.fromList
        [ ("title", Pandoc.toMetaValue contribution.title)
        , ("author", Pandoc.toMetaValue contribution.author)
        , ("date", Pandoc.toMetaValue $ Time.showGregorian contribution.date)
        , ("body", Pandoc.toMetaValue contribution.document)
        ]

--------------------------------------------------------------------------------

deriving anyclass instance Aeson.ToJSON (Bulletin Text ByteString)

deriving anyclass instance Aeson.FromJSON (Bulletin Text ByteString)

instance Aeson.ToJSON (Contribution ByteString) where
  toJSON contribution =
    Aeson.object
      [ "author" .= contribution.author
      , "title" .= contribution.title
      , "date" .= contribution.date
      , "format" .= contribution.format
      , "document" .= into @Base64ByteString contribution.document
      ]

instance Aeson.FromJSON (Contribution ByteString) where
  parseJSON = Aeson.withObject "Contribution ByteString" $ \o -> do
    author <- o .: "author"
    title <- o .: "title"
    date <- o .: "date"
    format <- o .: "format"
    document <- from @Base64ByteString <$> o .: "document"
    pure $ Contribution {author = author, title = title, date = date, format = format, document = document}

instance Aeson.ToJSON Base64ByteString where
  toJSON (Base64ByteString base64) = Aeson.String $ Text.decodeUtf8 $ Base64.extractBase64 base64

instance Aeson.FromJSON Base64ByteString where
  parseJSON = \case
    Aeson.String x -> pure $ Base64ByteString $ Base64.assertBase64 $ Text.encodeUtf8 x
    _ -> fail "Base 64-encoded value should be a string"

instance Aeson.ToJSON Base64Text where
  toJSON (Base64Text base64) = Aeson.String $ Base64.extractBase64 base64

instance Aeson.FromJSON Base64Text where
  parseJSON = \case
    Aeson.String x -> pure $ Base64Text $ Base64.assertBase64 x
    _ -> fail "Base 64-encoded value should be a string"

instance Aeson.ToJSON (Output Text) where
  toJSON output =
    Aeson.object
      [ "format" .= output.format
      , "template" .= into @Base64Text output.template
      ]

instance Aeson.FromJSON (Output Text) where
  parseJSON = Aeson.withObject "Output ByteString" $ \o -> do
    format <- o .: "format"
    template <- from @Base64Text <$> o .: "template"
    pure $ Output {format = format, template = template}

instance Aeson.ToJSON OutputFormat where
  toJSON = \case
    OutputFormatPdf compiler ->
      Aeson.object
        [ "tag" .= ("pdf" :: Text)
        , "compiler" .= compiler
        ]
    OutputFormatOther format ->
      Aeson.object
        [ "tag" .= ("format" :: Text)
        , "format" .= format
        ]

instance Aeson.FromJSON OutputFormat where
  parseJSON = Aeson.withObject "OutputFormat" $ \o -> do
    tag <- o .: "tag"
    case tag :: Text of
      "pdf" -> OutputFormatPdf <$> o .: "compiler"
      "format" -> OutputFormatOther <$> o .: "format"
      _ -> fail $ "Unknown type: " <> Text.unpack tag
