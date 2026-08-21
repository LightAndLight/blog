{-# LANGUAGE FlexibleContexts #-}

module Blog.Error
  ( tomlResult
  , tomlErrorReport
  , sageErrorReport
  , templeTypeErrorMessage
  , templeTypeErrorReport
  )
where

import Blog.Diagnostic (DiagnosticReports (..), Reports (..))
import Control.Monad.Error.Class (MonadError, throwError)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.List (intercalate)
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
  LazyByteString ->
  Either Toml.TomlError a ->
  m a
tomlResult _file _body (Right x) = pure x
tomlResult file body (Left err) =
  throwError . DiagnosticReports file body $ tomlErrorReport err

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

templeTypeErrorMessage ::
  Temple.TypeError loc ->
  String
templeTypeErrorMessage err =
  case err of
    Temple.NotInScope _loc ->
      "not in scope"
    Temple.TypeMismatch _loc expected actual ->
      "expected " ++ Temple.renderType expected ++ ", got " ++ Temple.renderType actual
    Temple.UnexpectedFields _loc fields ->
      "unexpected fields: " ++ renderFields fields
    Temple.MissingFields _loc fields ->
      "missing fields: " ++ renderFields fields
    Temple.UnexpectedConstructors _loc ctors ->
      "unexpected constructors: " ++ renderConstructors ctors
    Temple.MissingConstructors _loc ctors ->
      "missing constructors: " ++ renderConstructors ctors
    Temple.ArityMismatch _loc expected actual ->
      "expected " ++ show expected ++ plural expected " argument" ++ ", got " ++ show actual
    Temple.KindMismatch _loc expected actual ->
      "expected kind " ++ Temple.renderKind expected ++ ", got " ++ Temple.renderKind actual
    Temple.NotRequirement _loc name ->
      "'" ++ Text.unpack name ++ "' is not a requirement"
    Temple.BlockBadRequirementType _loc ty ->
      "block cannot satisfy requirement of type " ++ Temple.renderType ty
    Temple.RequirementAlreadySatisfied _loc ->
      "requirement already satisfied"
    Temple.FileNotFound _loc ->
      "file not found"
    Temple.ParentParseError _loc _file _parseError ->
      "parse error in parent"
    Temple.ParentTypeError _loc _file _typeError ->
      "type error in parent"
    Temple.IncludeDisabled _loc ->
      "includes are disabled"
    Temple.IncludeParseError _loc _file _parseError ->
      "parse error in include"
    Temple.IncludeTypeError _loc _file _typeError ->
      "type error in include"
    Temple.NotParam _loc ->
      "not a parameter"
    Temple.ParamAlreadyBound _loc ->
      "parameter already bound"
  where
    plural n word = if n == 1 then word else word ++ "s"

    renderFields =
      intercalate ", " . fmap (\(name, ty) -> Text.unpack name ++ " : " ++ Temple.renderType ty)

    renderConstructors =
      intercalate ", "
        . fmap
          (\(name, tys) -> Text.unpack name ++ "(" ++ intercalate ", " (fmap Temple.renderType tys) ++ ")")

templeTypeErrorReport ::
  Monad m =>
  (Temple.TemplateRef -> String) ->
  (Temple.TemplateRef -> m LazyByteString) ->
  Temple.TypeError Temple.Offset ->
  m Reports
templeTypeErrorReport renderTemplateRef loadTemplateRef err =
  let
    simple e = pure . One $ emit (Temple.typeErrorLoc e) (templeTypeErrorMessage err)
  in
    case err of
      Temple.NotInScope{} ->
        simple err
      Temple.TypeMismatch{} ->
        simple err
      Temple.UnexpectedFields{} ->
        simple err
      Temple.MissingFields{} ->
        simple err
      Temple.UnexpectedConstructors{} ->
        simple err
      Temple.MissingConstructors{} ->
        simple err
      Temple.ArityMismatch{} ->
        simple err
      Temple.KindMismatch{} ->
        simple err
      Temple.NotRequirement{} ->
        simple err
      Temple.BlockBadRequirementType{} ->
        simple err
      Temple.RequirementAlreadySatisfied{} ->
        simple err
      Temple.FileNotFound{} ->
        simple err
      Temple.ParentParseError loc file parseError ->
        inResource loc (templeTypeErrorMessage err) file . One $
          Text.Diagnostic.Sage.parseError parseError
      Temple.ParentTypeError loc file typeError ->
        inResource loc (templeTypeErrorMessage err) file
          =<< templeTypeErrorReport renderTemplateRef loadTemplateRef typeError
      Temple.IncludeDisabled{} ->
        simple err
      Temple.IncludeParseError loc file parseError ->
        inResource loc (templeTypeErrorMessage err) file . One $
          Text.Diagnostic.Sage.parseError parseError
      Temple.IncludeTypeError loc file typeError ->
        inResource loc (templeTypeErrorMessage err) file
          =<< templeTypeErrorReport renderTemplateRef loadTemplateRef typeError
      Temple.NotParam{} ->
        simple err
      Temple.ParamAlreadyBound{} ->
        simple err
  where
    emit loc =
      Diagnostic.emit (Diagnostic.Offset $ Temple.getOffset loc) Diagnostic.Caret . fromString

    inResource loc message ref reports = do
      contents <- loadTemplateRef ref
      pure $ More (emit loc message) (fromString $ renderTemplateRef ref) contents reports
