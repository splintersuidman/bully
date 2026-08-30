{-# LANGUAGE OverloadedStrings #-}

module Bully where

import           Control.Lens            ((^.))
import           Control.Monad.Except    (ExceptT, MonadError (throwError),
                                          runExceptT)
import           Control.Monad.IO.Class  (MonadIO (liftIO))
import           Data.Bifunctor          (second)
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as BS8
import qualified Data.ByteString.Lazy    as BL
import           Data.Foldable           (for_)
import           Data.List               (intercalate)
import           Data.Map.Strict         (Map)
import qualified Data.Map.Strict         as Map
import           Data.Maybe              (fromMaybe)
import           Data.Semigroup          (Min (Min, getMin))
import           Data.Text               (Text)
import qualified Data.Text               as Text
import qualified Data.Text.Encoding      as Text
import qualified Data.Text.IO            as Text
import qualified Data.Text.Lazy          as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time               as Time
import qualified Network.HTTP.Client     as Http
import qualified Network.Wreq            as Wreq
import           System.Directory        (setCurrentDirectory)
import           System.Environment      (getArgs)
import           System.Exit             (die)
import           System.FilePath         (takeBaseName, takeDirectory)
import           Text.Pandoc
import qualified Text.Pandoc.Builder     as P
import           Text.Pandoc.Builder     (Blocks, ToMetaValue (toMetaValue))
import qualified Text.Pandoc.Extensions  as Pandoc
import qualified Text.Pandoc.Format      as Pandoc
import qualified Text.Pandoc.PDF         as Pandoc
import qualified Text.Pandoc.Readers     as Pandoc
import qualified Text.Pandoc.Transforms  as Pandoc
import qualified Text.Pandoc.UTF8        as Pandoc
import           Text.Pandoc.Walk        (query)
import qualified Text.Pandoc.Writers     as Pandoc
import           Toml                    (TomlDecodeError)

data Bulletin templ doc = Bulletin
  { bulletinTitle         :: !Text
  , bulletinDate          :: !Time.Day
  , bulletinExtra         :: !(Map Text Text)
  , bulletinOutputs       :: ![Output templ]
  , bulletinContributions :: ![Contribution doc]
  } deriving (Show)

mapBulletin :: (Output t -> Output t') -> (Contribution d -> Contribution d') -> Bulletin t d -> Bulletin t' d'
mapBulletin f g bulletin = bulletin
  { bulletinOutputs = fmap f (bulletinOutputs bulletin)
  , bulletinContributions = fmap g (bulletinContributions bulletin)
  }

mapBulletinM :: Monad m
             => (Output t -> m (Output t'))
             -> (Contribution d -> m (Contribution d'))
             -> Bulletin t d
             -> m (Bulletin t' d')
mapBulletinM f g bulletin = do
  outputs <- mapM f $ bulletinOutputs bulletin
  contributions <- mapM g $ bulletinContributions bulletin
  pure $ bulletin
    { bulletinOutputs = outputs
    , bulletinContributions = contributions
    }

data Output templ = Output
  { outputFilename :: !FilePath
  , outputFormat   :: !OutputFormat
  , outputTemplate :: templ
  } deriving (Show, Functor)

data OutputFormat
  = OutputFormat Text
    -- ^ Output format for all formats except PDF (supported by writers in 'Pandoc.writers')
  | OutputFormatPdf Text
    -- ^ PDF output with specified compiler (supported by 'Pandoc.makePDF')
  | OutputFormatUnspecified
  deriving (Show)

data Contribution doc = Contribution
  { contributionAuthor   :: !Text
  , contributionTitle    :: !Text
  , contributionDate     :: !Time.Day
  , contributionDocument :: doc
  } deriving (Show, Functor)

mapContribution' :: (Contribution a -> b) -> Contribution a -> Contribution b
mapContribution' f contribution = contribution
  { contributionDocument = f contribution }

data Input = Input
  { inputSource :: !Source
  , inputFormat :: !(Maybe Text)
  } deriving (Show)

data Source
  = SourceFile !FilePath
  | SourceUrl !Text
  | SourceGoogleDocs !Text
  | SourceGoogleDrive !Text
  deriving (Show)

data BulletinError
  = BulletinUnsupportedInputFormat Input
  | BulletinUnsupportedOutputFormat String
  | BulletinPandocError PandocError
  | BulletinTomlDecodeError [TomlDecodeError]
  | BulletinCompileError BL.ByteString
  | BulletinTemplateError String
  | BulletinUsageError String
  | BulletinUnsupportedPdfCompiler Text
  | BulletinCsvParseError String

instance Show BulletinError where
  show = \case
    BulletinUnsupportedInputFormat fmt -> "Unsupported file format for contribution: " <> show fmt
    BulletinUnsupportedOutputFormat fmt -> "Unsupported file format for output: " <> show fmt
    BulletinPandocError err -> "Pandoc error: " <> show err
    BulletinTomlDecodeError errs -> "Toml error(s): " <> intercalate "\n" (fmap show errs)
    BulletinCompileError err -> "Compile error: " <> Pandoc.toStringLazy err
    BulletinTemplateError err -> "Template error: " <> err
    BulletinUsageError err -> "Usage error: " <> err
    BulletinUnsupportedPdfCompiler compiler -> "Unsupported PDF compiler: " <> Text.unpack compiler
    BulletinCsvParseError err -> "CSV parse error: " <> err

newtype Bully a = Bully { unBully :: ExceptT BulletinError IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadError BulletinError)

runBully :: Bully a -> IO a
runBully mx = do
  res <- runExceptT $ unBully mx
  either (die . show) pure res

liftEither' :: MonadError e m => (e' -> e) -> Either e' a -> m a
liftEither' f = either (throwError . f) pure

liftMaybe :: MonadError e m => e -> Maybe a -> m a
liftMaybe err = maybe (throwError err) pure

liftPandocIO :: PandocIO a -> Bully a
liftPandocIO mx = do
  res <- liftIO $ runIO mx
  liftEither' BulletinPandocError res

formatFromInput :: Input -> FilePath -> Bully Pandoc.FlavoredFormat
formatFromInput input filename = case inputFormat input of
  Nothing     -> liftMaybe (BulletinUnsupportedInputFormat input) $ Pandoc.formatFromFilePaths [filename]
  Just format -> liftPandocIO $ Pandoc.parseFlavoredFormat format

readUrl :: Text -> Bully BL.ByteString
readUrl url = liftIO $ (^. Wreq.responseBody) <$> Wreq.get (Text.unpack url)

defaultGoogleDocsFormat :: Text
defaultGoogleDocsFormat  = "docx"

readGoogleDocs :: Text -> Maybe Text -> Bully (FilePath, BL.ByteString)
readGoogleDocs docId format = do
  let url = "https://docs.google.com/document/d/" <> docId <> "/export?format=" <> fromMaybe defaultGoogleDocsFormat format
  response <- liftIO $ Wreq.get $ Text.unpack url
  let contentDisposition = response ^. Wreq.responseHeader "Content-Disposition"
  let filename = Pandoc.toString $ BS8.takeWhile (/= '"') $ BS8.drop 1 $ BS8.dropWhile (/= '"') contentDisposition
  let body = response ^. Wreq.responseBody
  pure (filename, body)

readGoogleDrive :: Text -> Maybe Text -> Bully (FilePath, BL.ByteString)
readGoogleDrive fileId format = do
  let openUrl = "https://drive.google.com/open?id=" <> fileId
  openResponse <- liftIO $ Wreq.customHistoriedMethod "GET" $ Text.unpack openUrl

  let url = case Http.host (openResponse ^. Wreq.hrFinalRequest) of
        "docs.google.com" -> "https://docs.google.com/document/d/" <> fileId <> "/export?format=" <> fromMaybe defaultGoogleDocsFormat format
        -- For ‘regular’ files (not in a Google format), the host is "drive.google.com"
        _ -> "https://drive.usercontent.google.com/uc?id=" <> fileId <> "&export=download"

  response <- liftIO $ Wreq.get $ Text.unpack url
  let contentDisposition = response ^. Wreq.responseHeader "Content-Disposition"
  let filename = Pandoc.toString $ BS8.takeWhile (/= '"') $ BS8.drop 1 $ BS8.dropWhile (/= '"') contentDisposition
  let body = response ^. Wreq.responseBody
  pure (filename, body)

readInputLazy :: Input -> Bully (Pandoc.FlavoredFormat, BL.ByteString)
readInputLazy input = do
  (filename, content) <- case inputSource input of
    SourceFile filename      -> liftPandocIO $ (filename,) <$> readFileLazy filename
    SourceUrl url            -> (Text.unpack url,) <$> readUrl url
    SourceGoogleDocs docId   -> readGoogleDocs docId $ inputFormat input
    SourceGoogleDrive fileId -> readGoogleDrive fileId $ inputFormat input
  format <- formatFromInput input filename
  pure (format, content)

readPandoc :: Pandoc.Reader PandocIO -> Pandoc.Extensions -> Contribution BL.ByteString -> Bully (Contribution Pandoc)
readPandoc reader extensions contribution = do
  let input = contributionDocument contribution
  let readerOptions = def { readerExtensions = extensions }
  doc <- case reader of
    Pandoc.ByteStringReader r -> liftPandocIO $ r readerOptions input
    Pandoc.TextReader r -> liftPandocIO $ r readerOptions $ TL.toStrict $ TL.decodeUtf8 input
  pure $ doc <$ contribution

-- | Read a contribution and parse it into Pandoc's AST.
readContribution :: Contribution Input -> Bully (Contribution Pandoc)
readContribution contribution = do
  let input = contributionDocument contribution
  (format, content) <- readInputLazy input
  (reader, extensions) <- liftPandocIO $ Pandoc.getReader format
  readPandoc reader extensions $ content <$ contribution

-- | Obtain the minimal header level from the document, if any headers
-- are present.
minHeaderLevel :: Pandoc -> Maybe Int
minHeaderLevel = fmap getMin . query minHeader
  where
    minHeader :: Block -> Maybe (Min Int)
    minHeader (Header level _ _) = Just $ Min level
    minHeader _                  = Nothing

-- | Possibly correct the header levels, if top-level headers are used.
correctHeaderLevels :: Pandoc -> Pandoc
correctHeaderLevels p = case minHeaderLevel p of
  Just l | l == 1 -> Pandoc.headerShift 1 p
  _               -> p

-- | Add header to a contribution, consisting of a pagebreak, a header
-- with the title, and the author set in italic.
addContributionHeader :: Contribution Pandoc -> Pandoc
addContributionHeader contribution = case contributionDocument contribution of
  Pandoc meta blocks -> Pandoc meta $ [Header 1 nullAttr title, Para author] <> blocks
  where
    title = P.toList $ P.text $ contributionTitle contribution
    author = P.toList $ P.emph $ P.text $ contributionAuthor contribution

-- | Extract the blocks from a contribution document, discarding the
-- meta values.
extractBlocks :: Contribution Pandoc -> Blocks
extractBlocks contribution = case contributionDocument contribution of
  Pandoc _meta blocks -> P.fromList blocks

-- | Perform all necessary transformations to make an input
-- contribution ready to be used as part of output.
processContribution :: Contribution Pandoc -> Contribution Blocks
processContribution = mapContribution' extractBlocks . fmap correctHeaderLevels

-- | Read the contributions specified in the given bulletin
-- configuration.
readContributions :: Bulletin templ Input -> Bully (Bulletin templ Pandoc)
readContributions = mapBulletinM pure readContribution

-- | Process the contributions.
processContributions :: Bulletin templ Pandoc -> Bulletin templ Blocks
processContributions = mapBulletin id processContribution

instance ToMetaValue a => ToMetaValue (Contribution a) where
  toMetaValue contribution = toMetaValue @(Map Text MetaValue) $ Map.fromList
    [ ("title", toMetaValue $ contributionTitle contribution)
    , ("author", toMetaValue $ contributionAuthor contribution)
    , ("date", toMetaValue $ Time.showGregorian $ contributionDate contribution)
    , ("body", toMetaValue $ contributionDocument contribution)
    ]

-- | Compile the bulletin, concatenating the contributions together
-- into a single document and setting the required variables.
compileBulletin :: Bulletin templ Blocks -> Pandoc
compileBulletin bulletin
  = P.setTitle (P.text $ bulletinTitle bulletin)
  $ P.setDate (P.text $ Text.pack $ Time.showGregorian $ bulletinDate bulletin)
  $ P.setMeta "extra" (bulletinExtra bulletin)
  $ P.setMeta "contributions" (toMetaValue $ bulletinContributions bulletin)
  $ mempty

-- | Read and compile the template file specified in the output.
readTemplate :: Output FilePath -> Bully (Output (Template Text))
readTemplate output = do
  let templateFile = outputTemplate output
  templateText <- liftIO $ Text.readFile templateFile
  templateRes <- liftIO $ compileTemplate templateFile templateText
  template <- liftEither' BulletinTemplateError templateRes
  pure $ template <$ output

-- | Read and compile the template files specified in the outputs of
-- the bulletin.
readTemplates :: Bulletin FilePath doc -> Bully (Bulletin (Template Text) doc)
readTemplates = mapBulletinM readTemplate pure

formatFromOutput :: FilePath -> Maybe Text -> Bully Pandoc.FlavoredFormat
formatFromOutput filename = \case
  Nothing -> liftMaybe (BulletinUnsupportedOutputFormat $ filename) $
    Pandoc.formatFromFilePaths [filename]
  Just format -> liftPandocIO $ Pandoc.parseFlavoredFormat format

writeOutputNormal :: Pandoc -> Output (Template Text) -> Maybe Text -> Bully ()
writeOutputNormal doc output fmt = do
  format <- formatFromOutput (outputFilename output) fmt
  (writer, extensions) <- liftPandocIO $ Pandoc.getWriter format
  let writerOptions = def
        { writerExtensions = extensions
        , writerTemplate = Just (outputTemplate output)
        , writerColumns = maxBound
        }
  case writer of
    Pandoc.ByteStringWriter w -> liftIO . BL.writeFile (outputFilename output) =<< liftPandocIO (w writerOptions doc)
    Pandoc.TextWriter w       -> liftIO . Text.writeFile (outputFilename output) =<< liftPandocIO (w writerOptions doc)

compilerToWriter :: Text -> Bully (WriterOptions -> Pandoc -> PandocIO Text)
compilerToWriter compiler = case takeBaseName (Text.unpack compiler) of
  "wkhtmltopdf"  -> pure writeHtml5String
  "pagedjs-cli"  -> pure writeHtml5String
  "prince"       -> pure writeHtml5String
  "weasyprint"   -> pure writeHtml5String
  "typst"        -> pure writeTypst
  "pdfroff"      -> pure writeMs
  "groff"        -> pure writeMs
  "context"      -> pure writeLaTeX
  "tectonic"     -> pure writeLaTeX
  "latexmk"      -> pure writeLaTeX
  "lualatex"     -> pure writeLaTeX
  "lualatex-dev" -> pure writeLaTeX
  "pdflatex"     -> pure writeLaTeX
  "pdflatex-dev" -> pure writeLaTeX
  "xelatex"      -> pure writeLaTeX
  _              -> throwError $ BulletinUnsupportedPdfCompiler compiler

writeOutputPdf :: Pandoc -> Output (Template Text) -> Text -> Bully ()
writeOutputPdf doc output compiler = do
  writer <- compilerToWriter compiler
  let writerOptions = def
        { writerTemplate = Just (outputTemplate output)
        , writerColumns = maxBound
        }
  compileRes <- liftPandocIO $ Pandoc.makePDF (Text.unpack compiler) [] writer writerOptions doc
  out <- liftEither' BulletinCompileError compileRes
  liftIO $ BL.writeFile (outputFilename output) out

-- | Write the bulletin as a PDF file, compiling it with Typst.
writeOutput :: Pandoc -> Output (Template Text) -> Bully ()
writeOutput doc output = case outputFormat output of
  OutputFormatUnspecified  -> writeOutputNormal doc output Nothing
  OutputFormat format      -> writeOutputNormal doc output (Just format)
  OutputFormatPdf compiler -> writeOutputPdf doc output compiler

writeOutputs :: Bulletin (Template Text) doc -> Pandoc -> Bully ()
writeOutputs bulletin doc = for_ (bulletinOutputs bulletin) $ writeOutput doc
