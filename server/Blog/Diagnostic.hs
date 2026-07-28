{-# LANGUAGE FlexibleContexts #-}

module Blog.Diagnostic
  ( DiagnosticReports (..)
  , renderDiagnosticReports
  , tomlResult
  , tomlErrorReport
  , sageErrorReport
  , templeTypeErrorReport
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
import Data.List (intercalate)

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

templeTypeErrorReport :: Temple.TypeError Temple.Offset -> IO Reports
templeTypeErrorReport err =
  case err of
    Temple.NotInScope loc ->
      pure . One $ emit loc "not in scope"
    Temple.TypeMismatch loc expected actual ->
      pure . One . emit loc $
        "expected '" ++ Temple.renderType expected ++ "', got '" ++ Temple.renderType actual ++ "'"
    Temple.UnexpectedFields loc fields ->
      pure . One . emit loc $ "unexpected fields: " ++ renderFields fields
    Temple.MissingFields loc fields ->
      pure . One . emit loc $ "missing fields: " ++ renderFields fields
    Temple.UnexpectedConstructors loc ctors ->
      pure . One . emit loc $ "unexpected constructors: " ++ renderConstructors ctors
    Temple.MissingConstructors loc ctors ->
      pure . One . emit loc $ "missing constructors: " ++ renderConstructors ctors
    Temple.ArityMismatch loc expected actual ->
      pure . One . emit loc $
        "expected " ++ show expected ++ plural expected " argument" ++ ", got " ++ show actual
    Temple.KindMismatch loc expected actual ->
      pure . One . emit loc $
        "expected kind '" ++ Temple.renderKind expected ++ "', got '" ++ Temple.renderKind actual ++ "'"
    Temple.NotRequirement loc name ->
      pure . One . emit loc $ "'" ++ Text.unpack name ++ "' is not a requirement"
    Temple.BlockBadRequirementType loc ty ->
      pure . One . emit loc $
        "block cannot satisfy requirement of type '" ++ Temple.renderType ty ++ "'"
    Temple.RequirementAlreadySatisfied loc ->
      pure . One $ emit loc "requirement already satisfied"
    Temple.FileNotFound loc ->
      pure . One $ emit loc "file not found"
    Temple.ParentParseError loc file parseError ->
      inFile loc "parse error in parent" file . One $
        Text.Diagnostic.Sage.parseError parseError
    Temple.ParentTypeError loc file typeError ->
      inFile loc "type error in parent" file =<< templeTypeErrorReport typeError
    Temple.IncludeDisabled loc ->
      pure . One $ emit loc "includes are disabled"
    Temple.IncludeParseError loc file parseError ->
      inFile loc "parse error in include" file . One $
        Text.Diagnostic.Sage.parseError parseError
    Temple.IncludeTypeError loc file typeError ->
      inFile loc "type error in include" file =<< templeTypeErrorReport typeError
    Temple.NotParam loc ->
      pure . One $ emit loc "not a parameter"
    Temple.ParamAlreadyBound loc ->
      pure . One $ emit loc "parameter already bound"
  where
    emit loc =
      Diagnostic.emit (Diagnostic.Offset $ Temple.getOffset loc) Diagnostic.Caret . fromString

    inFile loc message file reports = do
      contents <- LazyByteString.readFile file
      pure $ More (emit loc message) (fromString file) contents reports

    plural n word = if n == 1 then word else word ++ "s"

    renderFields =
      intercalate ", " . fmap (\(name, ty) -> Text.unpack name ++ " : " ++ Temple.renderType ty)

    renderConstructors =
      intercalate ", "
        . fmap (\(name, tys) -> unwords $ Text.unpack name : fmap Temple.renderType tys)
