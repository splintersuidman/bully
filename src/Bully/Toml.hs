{-# LANGUAGE OverloadedStrings #-}

module Bully.Toml where

import           Bully
import           Control.Applicative ((<|>))
import           Data.Text           (Text)
import qualified Toml
import           Toml                (TomlCodec, TomlDecodeError, (.=))

bulletinCodec :: TomlCodec (Bulletin FilePath Input)
bulletinCodec = Bulletin
  <$> Toml.text                             "title"        .= bulletinTitle
  <*> Toml.day                              "date"         .= bulletinDate
  <*> Toml.tableMap Toml._KeyText Toml.text "extra"        .= bulletinExtra
  <*> Toml.list outputCodec                 "output"       .= bulletinOutputs
  <*> Toml.list contributionCodec           "contribution" .= bulletinContributions

outputCodec :: TomlCodec (Output FilePath)
outputCodec = Output
  <$> Toml.string "file"     .= outputFilename
  <*> outputFormatCodec      .= outputFormat
  <*> Toml.string "template" .= outputTemplate

matchSourceFile :: Source -> Maybe FilePath
matchSourceFile = \case
  SourceFile file -> Just file
  _ -> Nothing

matchSourceUrl :: Source -> Maybe Text
matchSourceUrl = \case
  SourceUrl url -> Just url
  _ -> Nothing

matchSourceGoogleDocs :: Source -> Maybe Text
matchSourceGoogleDocs = \case
  SourceGoogleDocs docId -> Just docId
  _ -> Nothing

matchSourceGoogleDrive :: Source -> Maybe Text
matchSourceGoogleDrive = \case
  SourceGoogleDrive fileId -> Just fileId
  _ -> Nothing

sourceCodec :: TomlCodec Source
sourceCodec
   =  Toml.dimatch matchSourceFile SourceFile (Toml.string "file")
  <|> Toml.dimatch matchSourceUrl SourceUrl (Toml.text "url")
  <|> Toml.dimatch matchSourceGoogleDocs SourceGoogleDocs (Toml.text "google-docs")
  <|> Toml.dimatch matchSourceGoogleDrive SourceGoogleDrive (Toml.text "google-drive")

contributionCodec :: TomlCodec (Contribution Input)
contributionCodec = Contribution
  <$> Toml.text   "author" .= contributionAuthor
  <*> Toml.text   "title"  .= contributionTitle
  <*> Toml.day    "date"   .= contributionDate
  <*> inputCodec           .= contributionDocument

inputCodec :: TomlCodec Input
inputCodec = Input
  <$> sourceCodec                          .= inputSource
  <*> Toml.dioptional (Toml.text "format") .= inputFormat

matchOutputFormat :: OutputFormat -> Maybe Text
matchOutputFormat = \case
  OutputFormat format -> Just format
  _ -> Nothing

matchOutputFormatPdf :: OutputFormat -> Maybe Text
matchOutputFormatPdf = \case
  OutputFormatPdf compiler -> Just compiler
  _ -> Nothing

matchOutputFormatUnspecified :: OutputFormat -> Maybe ()
matchOutputFormatUnspecified = \case
  OutputFormatUnspecified -> Just ()
  _ -> Nothing

outputFormatCodec :: TomlCodec OutputFormat
outputFormatCodec
   =  Toml.dimatch matchOutputFormatPdf OutputFormatPdf pdf
  <|> Toml.dimatch matchOutputFormat OutputFormat otherFormat
  <|> pure OutputFormatUnspecified
  where
    pdf = Toml.hardcoded "pdf" Toml._Text "format" *> Toml.text "compiler"
    validateOtherFormat format = if format == "pdf"
      then Left "PDF output format should be accompanied by compiler option"
      else Right format
    otherFormat = Toml.validate validateOtherFormat Toml._Text "format"
