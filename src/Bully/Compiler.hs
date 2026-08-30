{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Bully.Compiler (
  Compiler,
  runCompiler,
  compile,
) where

import Bully.Types (Bulletin (..), Contribution (..), Output (..), OutputFormat (..), PdfCompiler (..))
import Control.Exception (Exception (..), throwIO)
import Control.Monad.Catch (MonadThrow, throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as BL
import Data.Semigroup (Min (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time qualified as Time
import Data.Vector qualified as Vector
import Text.Pandoc (Block (..), Pandoc (..), PandocError, PandocIO, ReaderOptions (..), WriterOptions (..))
import Text.Pandoc qualified as Pandoc
import Text.Pandoc.Builder (Blocks)
import Text.Pandoc.Builder qualified as P
import Text.Pandoc.Format qualified as Pandoc
import Text.Pandoc.Options (def)
import Text.Pandoc.PDF qualified as Pandoc
import Text.Pandoc.Templates (Template)
import Text.Pandoc.Templates qualified as Templates
import Text.Pandoc.Transforms qualified as Pandoc
import Text.Pandoc.Walk qualified as Pandoc
import Witch (From (..), into, via)

data CompileError
  = CompilePandocError PandocError
  | CompileTemplateError Text
  | CompileUnsupportedPdfCompilerError PdfCompiler
  | CompilePdfCompileError BL.ByteString
  deriving stock (Show)

instance Exception CompileError where
  displayException = \case
    CompilePandocError err -> "Pandoc error: " <> show err
    CompileTemplateError err -> "Template error: " <> from err
    CompileUnsupportedPdfCompilerError (PdfCompiler compiler) -> "Unsupported PDF compiler: " <> from compiler
    CompilePdfCompileError err -> "PDF compile error: " <> from (Text.decodeUtf8 $ from err)

newtype Compiler a = Compiler {unCompiler :: PandocIO a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

runCompiler :: Compiler a -> IO a
runCompiler (Compiler action) =
  either (throwIO . CompilePandocError) pure =<< Pandoc.runIO action
{-# INLINE runCompiler #-}

liftPandocIO :: PandocIO a -> Compiler a
liftPandocIO = Compiler

-- | Compile a 'Bulletin' and return the output.
compile :: Bulletin Text ByteString -> Compiler ByteString
compile bulletin = readContributions bulletin >>= readOutput >>= compileBulletin

compileBulletin :: Bulletin (Template Text) Blocks -> Compiler ByteString
compileBulletin bulletin =
  compilePandoc bulletin.output (makeBulletinDocument bulletin)

compilePandoc :: Output (Template Text) -> Pandoc -> Compiler ByteString
compilePandoc output document =
  case output.format of
    OutputFormatPdf pdfCompiler -> compilePandocPdf output.template pdfCompiler document
    OutputFormatOther format -> do
      flavoredFormat <- liftPandocIO $ Pandoc.parseFlavoredFormat format
      compilePandocOther output.template flavoredFormat document

compilePandocPdf :: Template Text -> PdfCompiler -> Pandoc -> Compiler ByteString
compilePandocPdf template pdfCompiler document = do
  writer <- pdfCompilerToWriter pdfCompiler
  let writerOptions =
        def
          { writerTemplate = Just template
          , writerColumns = maxBound
          }
  liftPandocIO $
    Pandoc.makePDF (via @Text pdfCompiler) [] writer writerOptions document
      >>= either (throwM . CompilePdfCompileError) (pure . into @ByteString)

pdfCompilerToWriter :: PdfCompiler -> Compiler (Pandoc.WriterOptions -> Pandoc -> PandocIO Text)
pdfCompilerToWriter compiler@(PdfCompiler c) = case c of
  "wkhtmltopdf" -> pure Pandoc.writeHtml5String
  "pagedjs-cli" -> pure Pandoc.writeHtml5String
  "prince" -> pure Pandoc.writeHtml5String
  "weasyprint" -> pure Pandoc.writeHtml5String
  "typst" -> pure Pandoc.writeTypst
  "pdfroff" -> pure Pandoc.writeMs
  "groff" -> pure Pandoc.writeMs
  "context" -> pure Pandoc.writeLaTeX
  "tectonic" -> pure Pandoc.writeLaTeX
  "latexmk" -> pure Pandoc.writeLaTeX
  "lualatex" -> pure Pandoc.writeLaTeX
  "lualatex-dev" -> pure Pandoc.writeLaTeX
  "pdflatex" -> pure Pandoc.writeLaTeX
  "pdflatex-dev" -> pure Pandoc.writeLaTeX
  "xelatex" -> pure Pandoc.writeLaTeX
  _ -> throwM $ CompileUnsupportedPdfCompilerError compiler

compilePandocOther
  :: Template Text
  -> Pandoc.FlavoredFormat
  -> Pandoc
  -> Compiler ByteString
compilePandocOther template format document = do
  (writer, extensions) <- liftPandocIO $ Pandoc.getWriter format
  let writerOptions =
        def
          { writerExtensions = extensions
          , writerTemplate = Just template
          , writerColumns = maxBound
          }
  case writer of
    Pandoc.ByteStringWriter w -> into @ByteString <$> liftPandocIO (w writerOptions document)
    Pandoc.TextWriter w -> Text.encodeUtf8 <$> liftPandocIO (w writerOptions document)

makeBulletinDocument :: Bulletin temp Blocks -> Pandoc
makeBulletinDocument bulletin =
  P.setTitle (P.text bulletin.title) $
    P.setDate (P.text $ from $ Time.showGregorian bulletin.date) $
      P.setMeta "extra" bulletin.extra $
        P.setMeta
          "contributions"
          (P.toMetaValue $ Vector.toList bulletin.contributions)
          mempty

readOutput :: Bulletin Text doc -> Compiler (Bulletin (Template Text) doc)
readOutput bulletin = do
  template <- readTemplate bulletin.output.template
  pure $ bulletin {output = template <$ bulletin.output}

readTemplate :: Text -> Compiler (Template Text)
readTemplate template =
  liftIO $
    Templates.compileTemplate "" template
      >>= either (throwM . CompileTemplateError . into @Text) pure

-- | Parse the contributions' documents into Pandoc's AST.
readContributions :: Bulletin temp ByteString -> Compiler (Bulletin temp Blocks)
readContributions bulletin = do
  contributions <- traverse readContribution bulletin.contributions
  pure $ bulletin {contributions = contributions}

-- | Parse a contribution's document into Pandoc's AST.
readContribution :: Contribution ByteString -> Compiler (Contribution Blocks)
readContribution contribution = do
  format <- liftPandocIO $ Pandoc.parseFlavoredFormat $ from contribution.format
  (reader, extensions) <- liftPandocIO $ Pandoc.getReader format
  doc <- readPandoc reader extensions contribution.document
  pure $ doc <$ contribution

readPandoc
  :: Pandoc.Reader PandocIO
  -> Pandoc.Extensions
  -> ByteString
  -> Compiler Blocks
readPandoc reader extensions doc = do
  let
    input = BL.fromStrict doc
    readerOptions = def {readerExtensions = extensions}
  document <- liftPandocIO $ case reader of
    Pandoc.ByteStringReader r -> r readerOptions input
    Pandoc.TextReader r -> r readerOptions $ TL.toStrict $ TL.decodeUtf8 input
  let processDocument = correctHeaderLevels
  case processDocument document of
    -- Discard meta values, because we set these manually for the compiled document.
    Pandoc _meta blocks -> pure $ P.fromList blocks

-- | Obtain the minimal header level from the document, if any headers are present.
minHeaderLevel :: Pandoc -> Maybe Int
minHeaderLevel = fmap getMin . Pandoc.query minHeader
 where
  minHeader :: Block -> Maybe (Min Int)
  minHeader (Header level _ _) = Just $ Min level
  minHeader _ = Nothing

-- | Possibly correct the header levels, if top-level headers are used.
correctHeaderLevels :: Pandoc -> Pandoc
correctHeaderLevels p = case minHeaderLevel p of
  Just l | l == 1 -> Pandoc.headerShift 1 p
  _ -> p
