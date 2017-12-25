module Impl.FastLogger (withHandle) where

import Service.Log as SvcLog
import System.Log.FastLogger
import Control.Monad.Logger

import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified System.Console.Terminal.Size as TermSize
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal

data Ann = AnnDebug
         | AnnInfo
         | AnnWarn
         | AnnError
         | AnnOther

toAnsi :: Ann -> AnsiStyle
toAnsi AnnDebug = color Cyan
toAnsi AnnInfo = color Green
toAnsi AnnWarn = color Yellow
toAnsi AnnError = color Red
toAnsi AnnOther = color Magenta

levelName :: LogLevel -> Text
levelName LevelDebug = "DEBUG"
levelName LevelInfo = "INFO"
levelName LevelWarn = "WARN"
levelName LevelError = "ERROR"
levelName (LevelOther o) = o

annotateLevel :: LogLevel -> Doc Ann -> Doc Ann
annotateLevel LevelDebug = annotate AnnDebug
annotateLevel LevelInfo = annotate AnnInfo
annotateLevel LevelWarn = annotate AnnWarn
annotateLevel LevelError = annotate AnnError
annotateLevel (LevelOther _) = annotate AnnOther

formatLog :: ToLogStr msg => Int -> Loc -> LogSource -> LogLevel -> msg -> FormattedTime -> LogStr
formatLog width loc src level msg time = toLogStr colorDocument <> " " <> toLogStr msg <> "\n"
  where
    colorDocument = renderStrict simpleDocAnsi
    layoutOptions = LayoutOptions { layoutPageWidth = AvailablePerLine width 0.4 }
    timestamp = lbracket <> pretty (TE.decodeUtf8 time) <> rbracket
    logLine = timestamp <> space <> pretty (levelName level)
    logDoc = annotateLevel level logLine
    simpleDocSemantic = layoutPretty layoutOptions logDoc
    simpleDocAnsi = reAnnotateS toAnsi simpleDocSemantic

withHandle :: (SvcLog.Handle -> IO a) -> IO a
withHandle action = do
  timeCache <- newTimeCache "%Y-%m-%d %H:%M:%S"
  termWidth :: Int <- TermSize.size >>= \case
    Nothing -> pure 80
    Just (TermSize.Window {TermSize.width = width}) -> pure width
  withTimedFastLogger timeCache (LogStderr defaultBufSize) $ \fl -> do
    -- action $ Handle (\log src level msg -> fl $ toLogStr msg <> "\n")
    action $ Handle (\loc src level msg -> fl $ formatLog termWidth loc src level msg)
