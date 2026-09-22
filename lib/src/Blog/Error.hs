{-# LANGUAGE FlexibleContexts #-}

module Blog.Error
  ( tomlResult
  , tomlErrorReport
  , sageErrorReport
  )
where

import Blog.Diagnostic (DiagnosticReports (..), Reports (..))
import Control.Monad.Error.Class (MonadError, throwError)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.String (fromString)
import qualified Data.Text as Text
import qualified Temple
import qualified Text.Diagnostic as Diagnostic
import qualified Text.Diagnostic.Sage
import qualified Toml

tomlResult ::
  MonadError DiagnosticReports m =>
  -- | File name
  ByteString ->
  -- | File contents
  ByteString ->
  Either Toml.TomlError a ->
  m a
tomlResult _file _body (Right x) = pure x
tomlResult file body (Left err) =
  throwError . DiagnosticReports file (LazyByteString.fromStrict body) $ tomlErrorReport err

sageErrorReport :: Temple.ParseError -> Reports
sageErrorReport =
  One . Text.Diagnostic.Sage.parseError

tomlErrorReport :: Toml.TomlError -> Reports
tomlErrorReport err =
  case err of
    Toml.ParseError err' ->
      One $ Text.Diagnostic.Sage.parseError err'
    Toml.DecodeFail offset ->
      One $
        Diagnostic.emit
          (Diagnostic.Offset offset)
          Diagnostic.Caret
          (fromString "decode failure")
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
    Toml.ExpectedDatetime offset ->
      One $
        Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret (fromString "expected a datetime")
    Toml.StringParseError offset string err' ->
      More
        (Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret (fromString "parse error in string"))
        (fromString "(string)")
        (LazyByteString.fromStrict string)
        (One $ Text.Diagnostic.Sage.parseError err')
    Toml.ExpectedRecord offset ->
      One $
        Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret (fromString "expected a record")
    Toml.MissingField offset name ->
      One $
        Diagnostic.emit
          (Diagnostic.Offset offset)
          Diagnostic.Caret
          (fromString $ "missing field '" ++ Text.unpack name ++ "'")
    Toml.UnexpectedFields fieldOffsets ->
      One $
        foldMap
          ( \offset -> Diagnostic.emit (Diagnostic.Offset offset) Diagnostic.Caret (fromString "unexpected field")
          )
          fieldOffsets
