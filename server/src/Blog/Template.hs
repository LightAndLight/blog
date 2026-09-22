{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Blog.Template
  ( -- * Loading and rendering templates
    loadTemplate
  , renderTemplate
  , parseTemplate
  , inferBindings

    -- ** Environment with blog-specific builtins
  , mkInferEnv
  , mkEvalEnv

    -- * Values and fields
  , boolValue
  , intValue
  , stringValue
  , textValue
  , bytestringValue
  , optionalValue
  , recordValue
  , Value (..)
  , fields
  , Fields (..)

    -- ** Providing values
  , ProviderT (..)
  , runProviderT
  , provide
  , provideTypeScheme
  , provideBinding

    -- ** Errors
  , ProviderError (..)
  , providerErrorDiagnostic
  , Part (..)
  , renderProviderPath
  , templeTypeErrorMessage
  , templeTypeErrorReport
  ) where

import Blog (ResourceId (..), renderName, renderResourceId, resourceName, resourceType, unsafeName)
import Blog.Build (ActionT, ResourceInput (..), resourceInputId)
import Blog.Diagnostic (DiagnosticReports (..), Reports (..))
import Blog.Error (sageErrorReport)
import qualified Blog.Store as Store
import Control.Applicative ((<|>))
import Control.Monad (guard, (<=<))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.Except (ExceptT, runExceptT, tryError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Int (Int64)
import Data.List (intercalate)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import Data.Time.Format (TimeLocale (months), defaultTimeLocale)
import Data.Traversable (for)
import GHC.Stack (HasCallStack)
import qualified Temple
import qualified Text.Diagnostic as Diagnostic
import qualified Text.Diagnostic.Sage

parseTemplate ::
  MonadError DiagnosticReports m => String -> ByteString -> m (Temple.Template Temple.Offset)
parseTemplate location input =
  case Temple.parse input of
    Left err ->
      throwError $
        DiagnosticReports
          (fromString location)
          (LazyByteString.fromStrict input)
          (sageErrorReport err)
    Right x -> pure x

inferBindings ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  (Temple.TemplateRef -> m (Maybe ByteString)) ->
  (Temple.TemplateRef -> String) ->
  (Temple.TemplateRef -> m ByteString) ->
  -- | Location, content (for error reporting)
  (String, ByteString) ->
  Temple.TemplateRef ->
  Temple.Template Temple.Offset ->
  m (Map Temple.TemplateRef Temple.Core, [Temple.Binding], Temple.Core)
inferBindings readTemplateRef renderTemplateRef getTemplateRef (location, input) inputRef template = do
  result <- runExceptT $ Temple.inferBindings (mkInferEnv readTemplateRef inputRef) template
  case result of
    Right x -> pure x
    Left err -> do
      throwError
        =<< DiagnosticReports
          (fromString location)
          (LazyByteString.fromStrict input)
          <$> templeTypeErrorReport renderTemplateRef getTemplateRef err

loadTemplate ::
  MonadIO m =>
  (Temple.TemplateRef -> ActionT m (Maybe ByteString)) ->
  (Temple.TemplateRef -> String) ->
  ResourceInput m ByteString ->
  ActionT m (Map.Map Temple.TemplateRef Temple.Core, [Temple.Binding], Temple.Core)
loadTemplate readTemplateRef renderTemplateRef iTemplate = do
  let inputTemplateRef = Temple.TemplateRef . renderName . resourceName $ resourceInputId iTemplate
  let templateResourceType = resourceType $ resourceInputId iTemplate

  let templateContent = resourceInputContent iTemplate
  template' <- parseTemplate (renderTemplateRef inputTemplateRef) templateContent

  let
    getTemplateRef (Temple.TemplateRef name) =
      fromMaybe
        (error $ "missing resource " ++ renderResourceId (ResourceId templateResourceType $ unsafeName name))
        <$> Store.readResource (resourceInputType iTemplate) (unsafeName name)

  inferBindings
    readTemplateRef
    renderTemplateRef
    getTemplateRef
    ( "(" ++ renderResourceId (resourceInputId iTemplate) ++ ")"
    , resourceInputContent iTemplate
    )
    inputTemplateRef
    template'

builtins :: Map Text (Temple.TypeScheme, Temple.Value)
builtins =
  Map.fromList
    [
      ( fromString "iso8601"
      ,
        ( Temple.Forall [] $ Temple.TFn [datetimeTy] Temple.TString
        , Temple.VFn . Temple.Fn $
            \case
              [Temple.VRecord datetime]
                | Just (Temple.VInt y) <- Map.lookup (fromString "year") datetime
                , Just (Temple.VInt m) <- Map.lookup (fromString "month") datetime
                , Just (Temple.VInt d) <- Map.lookup (fromString "day") datetime
                , Just (Temple.VInt hour) <- Map.lookup (fromString "hour") datetime
                , Just (Temple.VInt minute) <- Map.lookup (fromString "minute") datetime
                , Just (Temple.VInt second) <- Map.lookup (fromString "second") datetime ->
                    Temple.VString . fromString $
                      padZero 4 (show y)
                        ++ "-"
                        ++ padZero 2 (show m)
                        ++ "-"
                        ++ padZero 2 (show d)
                        ++ "T"
                        ++ padZero 2 (show hour)
                        ++ ":"
                        ++ padZero 2 (show minute)
                        ++ ":"
                        ++ padZero 2 (show second)
                        ++ "Z"
              _ -> undefined
        )
      )
    ,
      ( fromString "display-datetime"
      ,
        ( Temple.Forall [] $ Temple.TFn [datetimeTy] Temple.TString
        , Temple.VFn . Temple.Fn $
            \case
              [Temple.VRecord datetime]
                | Just (Temple.VInt y) <- Map.lookup (fromString "year") datetime
                , Just (Temple.VInt m) <- Map.lookup (fromString "month") datetime
                , Just (Temple.VInt d) <- Map.lookup (fromString "day") datetime
                , Just (Temple.VInt _hour) <- Map.lookup (fromString "hour") datetime
                , Just (Temple.VInt _minute) <- Map.lookup (fromString "minute") datetime
                , Just (Temple.VInt _second) <- Map.lookup (fromString "second") datetime ->
                    Temple.VString . fromString $
                      show d
                        ++ " "
                        ++ (fst $ months defaultTimeLocale !! (fromIntegral m - 1))
                        ++ ", "
                        ++ show y
              _ -> undefined
        )
      )
    ]
  where
    datetimeTy =
      Temple.TRecord
        $ Temple.TRecordField (fromString "year") Temple.TInt
          . Temple.TRecordField (fromString "month") Temple.TInt
          . Temple.TRecordField (fromString "day") Temple.TInt
          . Temple.TRecordField (fromString "hour") Temple.TInt
          . Temple.TRecordField (fromString "minute") Temple.TInt
          . Temple.TRecordField (fromString "second") Temple.TInt
        $ Temple.TRowEnd

    padZero n str = replicate (max 0 $ n - length str) '0' ++ str

mkInferEnv ::
  (Temple.TemplateRef -> m (Maybe ByteString)) ->
  Temple.TemplateRef ->
  Temple.InferEnv m
mkInferEnv readTemplateRef currentTemplate = env{Temple.ieScope = fmap fst builtins <> Temple.ieScope env}
  where
    env = (Temple.defaultInferEnv readTemplateRef currentTemplate)

mkEvalEnv :: Map Temple.TemplateRef Temple.Core -> Temple.EvalEnv
mkEvalEnv deps = env{Temple.eeScope = fmap snd builtins <> Temple.eeScope env}
  where
    env = Temple.defaultEvalEnv deps

renderTemplate ::
  MonadIO m =>
  ResourceInput m ByteString ->
  Fields (ActionT m) ->
  ActionT m LazyByteString
renderTemplate iTemplate values = do
  let templateResourceType = resourceType $ resourceInputId iTemplate

  let
    readTemplateRef (Temple.TemplateRef name) =
      Store.readResource (resourceInputType iTemplate) (unsafeName name)

  let
    renderTemplateRef (Temple.TemplateRef name) =
      renderResourceId (ResourceId templateResourceType (unsafeName name))

  let
    getTemplateRef (Temple.TemplateRef name) =
      fromMaybe
        (error $ "missing resource " ++ renderResourceId (ResourceId templateResourceType (unsafeName name)))
        <$> Store.readResource (resourceInputType iTemplate) (unsafeName name)

  (deps, bindings, template'') <- loadTemplate readTemplateRef renderTemplateRef iTemplate

  let currentTemplate = Temple.TemplateRef . renderName . resourceName $ resourceInputId iTemplate
  bindings' <- for bindings $ \binding ->
    (,) (Temple.bindingName binding)
      <$> runProviderT
        renderTemplateRef
        getTemplateRef
        ("(" ++ renderResourceId (resourceInputId iTemplate) ++ ")")
        (provideBinding readTemplateRef currentTemplate binding values)

  pure $ Temple.evalTemplate (mkEvalEnv deps) template'' bindings'

data Part
  = PField Text
  | PIndex Int
  | PCtorArg Text Int

data ProviderError
  = ParameterNotFound
      -- | Template being instantiated
      (Maybe Temple.TemplateRef)
      -- | Template that introduced parameter
      Temple.TemplateRef
      -- | Offset of parameter
      Temple.Offset
  | ResourceTypeNotFound ![Part] !Text
  | ResourceNotFound ![Part] !Text
  | PropertyNotFound ![Part] !Text
  | TypeError ![Part] (Temple.TypeError ())

data Value m
  = Value
      -- | A value
      Temple.Core
      -- | The value's type
      Temple.Type
  | Record (Fields m)
  | List [Value m]
  | Constructor Text [Value m]

newtype Fields m = Fields {getField :: [Part] -> Text -> ProviderT m (Maybe (Value m))}

instance Monad m => Semigroup (Fields m) where
  Fields f <> Fields g = Fields $ \x y -> runMaybeT $ MaybeT (f x y) <|> MaybeT (g x y)

boolValue :: Bool -> Value m
boolValue True = Value Temple.CTrue Temple.TBool
boolValue False = Value Temple.CFalse Temple.TBool

intValue :: (HasCallStack, Integral a) => a -> Value m
intValue n =
  let
    nInteger :: Integer
    nInteger = fromIntegral n

    !n64
      | fromIntegral (minBound :: Int64) <= nInteger
      , nInteger <= fromIntegral (maxBound :: Int64) =
          fromIntegral n :: Int64
      | otherwise = error $ show nInteger ++ " exceeds the bounds of Int64"
  in
    Value (Temple.CInt n64) Temple.TInt

stringValue :: String -> Value m
stringValue value =
  Value
    (Temple.CString [Temple.CPartText . Text.Encoding.encodeUtf8 $ fromString value])
    Temple.TString

textValue :: Text -> Value m
textValue value = Value (Temple.CString [Temple.CPartText $ Text.Encoding.encodeUtf8 value]) Temple.TString

bytestringValue :: ByteString -> Value m
bytestringValue value = Value (Temple.CString [Temple.CPartText value]) Temple.TString

optionalValue :: Maybe (Value m) -> Value m
optionalValue Nothing = Constructor (fromString "None") []
optionalValue (Just value) = Constructor (fromString "Some") [value]

recordValue :: Monad m => [(Text, Value m)] -> Value m
recordValue = Record . fields

fields :: Monad m => [(Text, Value m)] -> Fields m
fields fs = Fields $ \_path name -> pure $ Map.lookup name fs'
  where
    fs' = Map.fromList fs

unifyProvidedType ::
  Monad m =>
  -- | Path to type
  [Part] ->
  -- | Expected
  Temple.Type ->
  -- | Actual
  Temple.Type ->
  Temple.InferT () (ProviderT m) ()
unifyProvidedType path a b = do
  result <- tryError $ Temple.unify () a b
  case result of
    Left err -> lift . throwError $ TypeError path err
    Right x -> pure x

-- TODO: expose in `temple`?
getRecordFields :: Temple.Type -> ([(Text, Temple.Type)], Maybe (Temple.Type))
getRecordFields Temple.TRowEnd =
  ([], Nothing)
getRecordFields ty'@Temple.TMeta{} =
  ([], pure ty')
getRecordFields ty'@Temple.TVar{} =
  ([], pure ty')
getRecordFields (Temple.TRecordField name ty' rest) =
  first ((name, ty') :) $ getRecordFields rest
getRecordFields ty' =
  error $ "unexpected type in record:" ++ show ty'

newtype ProviderT m a
  = ProviderT (ExceptT ProviderError m a)
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO, MonadError ProviderError)

runProviderT ::
  (MonadError DiagnosticReports m, MonadIO m) =>
  -- | Render a template reference
  (Temple.TemplateRef -> String) ->
  -- | Read a template's contents
  (Temple.TemplateRef -> m ByteString) ->
  -- | Location name (for error reporting)
  String ->
  ProviderT m a ->
  m a
runProviderT renderTemplateRef getTemplateRef location (ProviderT ma) =
  either (throwError <=< providerErrorDiagnostic renderTemplateRef getTemplateRef location) pure
    =<< runExceptT ma

provide ::
  Monad m =>
  -- | Template parameter name
  Text ->
  -- | Required type
  Temple.Type ->
  -- | Provided value
  Value m ->
  Temple.InferT () (ProviderT m) Temple.Core
provide param = go [PField param]
  where
    go path ty (Value core actualTy) = core <$ unifyProvidedType path ty actualTy
    go path ty (Record providedFields) = do
      fieldsTy <- Temple.metavar Temple.KRow
      unifyProvidedType path ty (Temple.TRecord fieldsTy)
      (knownFields, _rest) <- getRecordFields <$> Temple.zonkNoDefault fieldsTy
      fields' <- for knownFields $ \(fieldName, fieldTy) -> do
        mProvidedField <- lift $ getField providedFields path fieldName
        case mProvidedField of
          Nothing ->
            lift . throwError $ PropertyNotFound path fieldName
          Just providedField -> do
            core <- go (path <> pure (PField fieldName)) fieldTy providedField
            pure (fieldName, core)
      pure $ Temple.CRecord fields'
    go path ty (List items) = do
      itemTy <- Temple.metavar Temple.KType
      unifyProvidedType path ty (Temple.TStream itemTy)
      items' <- for (zip [0 ..] items) $ \(ix, item) -> do
        go (path <> pure (PIndex ix)) itemTy item
      pure $ Temple.CArray items'
    go path ty (Constructor name args) = do
      argTys <- traverse (const $ Temple.metavar Temple.KType) args
      sumRow <- Temple.metavar Temple.KRow
      unifyProvidedType path ty (Temple.TSum $ Temple.TSumConstructor name argTys sumRow)
      argTys' <- traverse Temple.zonkNoDefault argTys
      args' <- for (zip3 [0 ..] args argTys') $ \(ix, arg, argTy) -> do
        go (path <> pure (PCtorArg name ix)) argTy arg
      pure $ Temple.CConstructor name args'

provideTypeScheme ::
  Monad m =>
  -- | Read a template's contents
  (Temple.TemplateRef -> m (Maybe ByteString)) ->
  -- | Template being instantiated
  Temple.TemplateRef ->
  -- | Template parameter name
  Text ->
  -- | Required type
  Temple.TypeScheme ->
  -- | Provided value
  Value m ->
  ProviderT m Temple.Core
provideTypeScheme readTemplateRef currentTemplate name scheme value = do
  result <- Temple.runInferT
    (Temple.emptyInferEnv (lift . readTemplateRef) currentTemplate)
    Temple.emptyInferState
    $ do
      ty <- Temple.instantiateTypeScheme scheme
      provide name ty value
  either (throwError . TypeError [PField name]) (pure . snd) result

provideBinding ::
  Monad m =>
  -- | Read a template's contents
  (Temple.TemplateRef -> m (Maybe ByteString)) ->
  -- | Template being instantiated
  Temple.TemplateRef ->
  -- | Binding to satisfy
  Temple.Binding ->
  -- | Provided values
  Fields m ->
  ProviderT m Temple.Core
provideBinding readTemplateRef currentTemplate binding values = do
  let name = Temple.bindingName binding
  mValue <- getField values [PField $ Temple.bindingName binding] name
  case mValue of
    Just value ->
      provideTypeScheme readTemplateRef currentTemplate name (Temple.bindingScheme binding) value
    Nothing -> do
      let (bindingRef, bindingOffset) = NonEmpty.head $ Temple.bindingLocations binding
      throwError $
        ParameterNotFound
          (currentTemplate <$ guard (bindingRef /= currentTemplate))
          bindingRef
          bindingOffset

renderProviderPath :: [Part] -> String
renderProviderPath [] = ""
renderProviderPath [p] = renderPart p
renderProviderPath (p : ps@(p' : _)) =
  renderPart p
    ++ (case p' of PField{} -> "."; PCtorArg{} -> "."; PIndex{} -> "")
    ++ renderProviderPath ps

renderPart :: Part -> String
renderPart (PField f)
  | Text.elem '.' f = "`" ++ Text.unpack f ++ "`"
  | otherwise = Text.unpack f
renderPart (PIndex n) = "[" ++ show n ++ "]"
renderPart (PCtorArg name ix) = Text.unpack name ++ "(" ++ show ix ++ ")"

providerErrorDiagnostic ::
  MonadIO m =>
  (Temple.TemplateRef -> String) ->
  (Temple.TemplateRef -> m ByteString) ->
  -- | Error location name
  String ->
  ProviderError ->
  m DiagnosticReports
providerErrorDiagnostic renderTemplateRef getTemplateRef location err =
  -- TODO: point to the location in the template?
  case err of
    ParameterNotFound mOrigin ref offset -> do
      content <- getTemplateRef ref
      pure $
        DiagnosticReports
          (fromString $ "(" ++ renderTemplateRef ref ++ ")")
          (LazyByteString.fromStrict content)
          ( One $
              Diagnostic.emit
                (Diagnostic.Offset $ Temple.getOffset offset)
                Diagnostic.Caret
                ( fromString $
                    "not in scope"
                      ++ foldMap ((" (while instantiating " ++) . (++ ")") . renderTemplateRef) mOrigin
                )
          )
    ResourceTypeNotFound path name ->
      pure . DiagnosticSimple $
        location
          ++ ": "
          ++ renderProviderPath path
          ++ ": missing resource type '"
          ++ Text.unpack name
          ++ "'"
    ResourceNotFound path name ->
      pure . DiagnosticSimple $
        location
          ++ ": "
          ++ renderProviderPath path
          ++ ": missing resource '"
          ++ Text.unpack name
          ++ "'"
    PropertyNotFound path name ->
      pure . DiagnosticSimple $
        location
          ++ ": "
          ++ renderProviderPath path
          ++ ": missing property '"
          ++ Text.unpack name
          ++ "'"
    TypeError path err' ->
      pure . DiagnosticSimple $
        location ++ ": " ++ renderProviderPath path ++ ": " ++ templeTypeErrorMessage err'

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
  (Temple.TemplateRef -> m ByteString) ->
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
      pure $
        More
          (emit loc message)
          (fromString $ renderTemplateRef ref)
          (LazyByteString.fromStrict contents)
          reports
