{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Bully.Types (Bulletin (..), Contribution (..), ContributionFormat (..), Output (..), OutputFormat (..), PdfCompiler (..))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Vector qualified as Vector

main :: IO ()
main = do
  template <- Text.readFile "example/template.typ"
  contribution1 <- BS.readFile "example/contribution1.md"
  contribution2 <- BS.readFile "example/contribution2.md"

  let bulletin :: Bulletin Text ByteString =
        Bulletin
          { title = "Example bulletin"
          , date = read "2026-01-01"
          , extra = mempty
          , contributions =
              Vector.fromList
                [ Contribution
                    { author = "First author"
                    , title = "Contribution one"
                    , date = read "2025-12-31"
                    , format = ContributionFormat "markdown"
                    , document = contribution1
                    }
                , Contribution
                    { author = "Second author"
                    , title = "Contribution two"
                    , date = read "2026-01-01"
                    , format = ContributionFormat "markdown"
                    , document = contribution2
                    }
                ]
          , output =
              Output
                { format = OutputFormatPdf (PdfCompiler "typst")
                , template = template
                }
          }
  BL.writeFile "example/request.txt" $ Aeson.encode bulletin
