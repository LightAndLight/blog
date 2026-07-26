{-# LANGUAGE FlexibleContexts #-}

module Blog.Diagnostic
  ( DiagnosticReports (..)
  , renderDiagnosticReports
  , tomlResult
  , tomlErrorReport
  , sageErrorReport
  )
where

import Control.Monad.Error.Class (MonadError, throwError)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Temple
import qualified Text.Diagnostic as Diagnostic
import qualified Text.Diagnostic.Sage
import qualified Toml

data DiagnosticReports
  = DiagnosticReports
      -- | File name
      ByteString
      -- | File contents
      LazyByteString
      Reports
  | DiagnosticSimple String

renderDiagnosticReports :: DiagnosticReports -> LazyByteString
renderDiagnosticReports (DiagnosticReports file contents reports) = go file contents reports
  where
    go file' contents' (One report') =
      Diagnostic.render Diagnostic.defaultConfig{Diagnostic.colors = Nothing} file' contents' report'
    go file' contents' (More report' file'' contents'' reports'') =
      Diagnostic.render Diagnostic.defaultConfig{Diagnostic.colors = Nothing} file' contents' report'
        <> fromString "\n"
        <> go file'' contents'' reports''
renderDiagnosticReports (DiagnosticSimple s) = fromString $ "error: " ++ s

tomlResult ::
  MonadError DiagnosticReports m =>
  -- | File name
  ByteString ->
  LazyByteString ->
  Either Toml.TomlError a ->
  m a
tomlResult _file _body (Right x) = pure x
tomlResult file body (Left err) =
  throwError . DiagnosticReports file body $ tomlErrorReport err

data Reports
  = One Diagnostic.Report
  | More
      -- | Current report
      Diagnostic.Report
      -- | File name for next report
      ByteString
      -- | Content for next report
      LazyByteString
      -- | Next report
      Reports

sageErrorReport :: Temple.ParseError -> Reports
sageErrorReport =
  One . Text.Diagnostic.Sage.parseError

tomlErrorReport :: Toml.TomlError -> Reports
tomlErrorReport err =
  case err of
    Toml.ParseError err' ->
      One $ Text.Diagnostic.Sage.parseError err'
    Toml.MissingKey offset name ->
      One $
        Diagnostic.emit
          (Diagnostic.Offset offset)
          Diagnostic.Caret
          (fromString $ "missing key '" ++ Text.unpack name ++ "'")
    Toml.DuplicateKey offset _name ->
      One $ Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret (fromString "duplicate key")
    Toml.MissingTable offset name ->
      One $
        Diagnostic.emit
          (Diagnostic.Offset offset)
          Diagnostic.Caret
          (fromString $ "missing table '" ++ Text.unpack name ++ "'")
    Toml.DuplicateTables offsets _name ->
      One $
        foldMap
          ( \offset ->
              Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret (fromString "duplicate table")
          )
          offsets
    Toml.UnexpectedEntries keys tableOffsets ->
      One $
        foldMap
          ( \(Toml.TomlKeyEntry offset _value) ->
              Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret (fromString "unexpected key")
          )
          keys
          <> foldMap
            ( \offset -> Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret (fromString "unexpected entry")
            )
            tableOffsets
    Toml.ExpectedString offset ->
      One $
        Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret (fromString "expected a string")
    Toml.StringParseError offset string err' ->
      More
        (Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret (fromString "parse error in string"))
        (fromString "(string)")
        (LazyByteString.fromStrict string)
        (One $ Text.Diagnostic.Sage.parseError err')
