{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Blog.Rules (rules) where

import Blog
  ( MetadataType (..)
  , MetadataValue (..)
  , ResourceId (..)
  , cfgMetadata
  , metaCfgDefault
  , metaCfgOptional
  , metaCfgType
  , metadataValueString
  , renderResourceId
  , resourceIdParser
  )
import Blog.Build ((*<))
import qualified Blog.Build as Build
import Blog.Diagnostic (DiagnosticReports (..), Reports (..))
import Blog.Error (sageErrorReport, templeTypeErrorMessage, templeTypeErrorReport, tomlResult)
import Blog.Metadata (parseResourceMetadata)
import Blog.Store (Store)
import qualified Blog.Store as Store
import Commonmark.Pandoc (Cm, unCm)
import Commonmark.Parser (commonmark)
import Control.Applicative ((<|>))
import Control.Monad (unless)
import Control.Monad.Error.Class (MonadError, throwError, tryError)
import Control.Monad.Except (ExceptT (..), mapExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT, tell)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString.Lazy.Char8
import Data.Foldable (fold, for_)
import Data.List (find, intercalate, sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (for)
import qualified Temple
import qualified Text.Diagnostic as Diagnostic
import Text.Pandoc.Builder (Blocks)
import qualified Text.Pandoc.Builder as Blocks (toList)
import qualified Text.Pandoc.Html as Html
import qualified Toml

rules :: MonadIO m => Build.Rules m
rules =
  Build.rule
    "template-dependency"
    (Build.iResource "template" Build.iAny)
    (pure ())
    templateDependency
    <> Build.rule
      "article-adjacency"
      (Build.iResourceAll "article" Build.iAny)
      (Build.oResource "adjacency" $ Build.oMatch "article-" *< Build.oAny)
      articleAdjacency
    <> Build.rule
      "article-html"
      ( (,,)
          <$> Build.iResource "template" (Build.iMatch "article.html.temple")
          <*> Build.iResource "article" (Build.iBind "name")
          <*> Build.iResource "adjacency" (Build.iMatch "article-" <> Build.iBind "name")
      )
      (Build.oResource "html" $ Build.oMatch "article-" *< Build.oBind "name")
      articleHtml

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

templateDependency ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m ->
  () ->
  Build.ActionT m ()
templateDependency iTemplate () = do
  let location = "(" ++ renderResourceId (Build.resourceInputId iTemplate) ++ ")"
  template' <-
    case Temple.parse (LazyByteString.toStrict $ Build.resourceInputContent iTemplate) of
      Left err ->
        throwError $
          DiagnosticReports
            (fromString location)
            (Build.resourceInputContent iTemplate)
            (sageErrorReport err)
      Right x -> pure x

  let
    templateResourceType = resourceType $ Build.resourceInputId iTemplate

  let
    renderTemplateRef (Temple.TemplateRef name) =
      "(" ++ renderResourceId (ResourceId templateResourceType name) ++ ")"

    readTemplateRef :: Temple.TemplateRef -> Build.ActionT m (Maybe ByteString)
    readTemplateRef (Temple.TemplateRef name) =
      fmap LazyByteString.toStrict <$> Store.readResource (Build.resourceInputType iTemplate) name

    getTemplateRef (Temple.TemplateRef name) =
      fromMaybe (error $ "missing resource " ++ renderResourceId (ResourceId templateResourceType name))
        <$> Store.readResource (Build.resourceInputType iTemplate) name

  let ref = Temple.TemplateRef . resourceName $ Build.resourceInputId iTemplate
  (deps, bindings) <- do
    result <- runExceptT $ Temple.inferBindings readTemplateRef ref template'
    case result of
      Right x -> pure x
      Left err -> do
        throwError
          =<< DiagnosticReports
            (fromString location)
            (Build.resourceInputContent iTemplate)
            <$> templeTypeErrorReport renderTemplateRef getTemplateRef err

  {-
  If a template statically depends on some resources, then we check that
  those resources exist when the template is uploaded. These resources are
  also registered as dependencies of the template.
  -}
  store <- Build.askStore
  xactId <- Build.askTransactionId
  mResources <-
    for (find ((fromString "resource" ==) . Temple.bindingName) bindings) $ \binding -> do
      let (bindingRef, _bindingOffset) = NonEmpty.head $ Temple.bindingLocations binding
      (result, resources) <- runWriterT . runExceptT $ do
        let path = pure $ Temple.bindingName binding
        let readTemplateRef' = lift . lift . readTemplateRef
        result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' bindingRef) Temple.emptyInferState $ do
          ty <- Temple.instantiateTypeScheme $ Temple.bindingScheme binding
          resourceTypeProvider store xactId path ty
        either (throwError . TypeError path) pure result
      case result of
        Right (_state, _value) -> pure resources
        Left err ->
          throwError
            =<< typeProviderErrorDiagnostic renderTemplateRef getTemplateRef (Build.resourceInputId iTemplate) err

  let
    templateDependencies =
      Set.map (\(Temple.TemplateRef r) -> ResourceId "template" r) (Map.keysSet deps)
        <> fold mResources
  Build.setDependencies (Build.resourceInputId iTemplate) templateDependencies

articleAdjacency ::
  Monad m =>
  Build.ResourceInputs m ->
  Build.ResourceOutput m String ->
  Build.ActionT m ()
articleAdjacency iArticles oAdjacency = do
  store <- Build.askStore
  xactId <- Build.askTransactionId

  resTy <- do
    let resTyName = Build.resourceInputsType iArticles
    Store.getResourceType store xactId resTyName
  articles' <- Store.listResource resTy
  articlesWithPublished <- for articles' $ \article -> do
    metadata <- do
      mMetadata <- Store.readResourceMetadata resTy $ resourceName article
      case mMetadata of
        Nothing ->
          throwError . DiagnosticSimple $ renderResourceId article ++ " has no metadata"
        Just metadata ->
          parseResourceMetadata
            (Store.resourceTypeConfig resTy)
            (Store.resourceTypeName resTy)
            (resourceName article)
            metadata

    published <- do
      input <- case Map.lookup (fromString "published") metadata of
        Nothing ->
          throwError . DiagnosticSimple $ renderResourceId article ++ "'s metadata has no 'published' field"
        Just x ->
          pure x
      let !input' = metadataValueString input
      let
        parseDateTime :: Text -> Maybe UTCTime
        parseDateTime = iso8601ParseM . Text.unpack

        parseDate :: Text -> Maybe UTCTime
        parseDate x = do
          day <- iso8601ParseM $ Text.unpack x
          pure $ UTCTime day 0
      case parseDateTime input' <|> parseDate input' of
        Nothing ->
          throwError . DiagnosticSimple $
            renderResourceId article ++ "'s metadata an invalid 'published' field: " ++ Text.unpack input'
        Just x -> pure (x :: UTCTime)
    pure (article, published)

  let sortedArticles = sortOn snd articlesWithPublished
  let adjacencies = makeAdjacencies sortedArticles

  for_ adjacencies $ \adjacency -> do
    let Adjacency prev (ResourceId _ currentResName) next = fmap fst adjacency
    let
      -- TODO: string escaping, move to `tomlin` library
      tomlString = (fromString "\"" <>) . (<> fromString "\"")
    Build.writeResource
      oAdjacency
      currentResName
      ( foldMap (<> fromString "\n") $
          [ fromString "previous = " <> tomlString (fromString $ renderResourceId resId) | Just resId <- [prev]
          ]
            ++ [fromString "next = " <> tomlString (fromString $ renderResourceId resId) | Just resId <- [next]]
      )

data Adjacency a
  = Adjacency
      -- | Previous
      (Maybe a)
      -- | Current
      a
      -- | Next
      (Maybe a)
  deriving (Functor)

makeAdjacencies :: [a] -> [Adjacency a]
makeAdjacencies xs =
  case xs of
    [] -> []
    [x] -> [Adjacency Nothing x Nothing]
    [x, y] -> [Adjacency Nothing x (Just y), Adjacency (Just x) y Nothing]
    x : y : z : rest -> Adjacency Nothing x (Just y) : go x y z rest
  where
    go prev current next rest =
      Adjacency (Just prev) current (Just next)
        : case rest of
          [] -> [Adjacency (Just current) next Nothing]
          next' : rest' -> go current next next' rest'

mkOptional :: Temple.Type -> Temple.Type
mkOptional t =
  Temple.TSum $
    Temple.TSumConstructor (fromString "Some") [t] $
      Temple.TSumConstructor (fromString "None") [] $
        Temple.TRowEnd

mkRecord :: [(String, Temple.Type)] -> Temple.Type
mkRecord = Temple.TRecord . foldr (\(name, ty) -> Temple.TRecordField (fromString name) ty) Temple.TRowEnd

metaToTempleTy :: MetadataType -> Temple.Type
metaToTempleTy TBool = Temple.TBool
metaToTempleTy TString = Temple.TString
metaToTempleTy (TList t) = Temple.TStream (metaToTempleTy t)

metaToTempleValue :: MetadataValue -> Temple.Value
metaToTempleValue (VString s) =
  Temple.VString . Text.Lazy.Encoding.encodeUtf8 $ LazyText.fromStrict s
metaToTempleValue VTrue =
  Temple.VTrue
metaToTempleValue VFalse =
  Temple.VFalse
metaToTempleValue (VList xs) =
  Temple.VStream (fmap metaToTempleValue xs)
metaToTempleValue (VConstructor name args) =
  Temple.VConstructor name (fmap metaToTempleValue args)

type TypeProvider m =
  [Text] -> Temple.Type -> Temple.InferT () (ExceptT TypeProviderError m) Temple.Value

data TypeProviderError
  = NotARecord
      ![Text]
      -- | Expected type
      !Temple.Type
  | ParameterNotFound Temple.TemplateRef Temple.Offset
  | ResourceTypeNotFound ![Text] !Text
  | ResourceNotFound ![Text] !Text
  | PropertyNotFound ![Text] !Text
  | TypeError ![Text] (Temple.TypeError ())

typeProviderErrorDiagnostic ::
  MonadIO m =>
  (Temple.TemplateRef -> String) ->
  (Temple.TemplateRef -> m LazyByteString) ->
  ResourceId ->
  TypeProviderError ->
  m DiagnosticReports
typeProviderErrorDiagnostic renderTemplateRef getTemplateRef resId err =
  -- TODO: point to the location in the template?
  case err of
    NotARecord path ty ->
      pure . DiagnosticSimple $
        renderTemplateId resId
          ++ renderTypeProviderPath path
          ++ "expected "
          ++ Temple.renderType ty
          ++ ", got a record"
    ParameterNotFound ref offset -> do
      content <- getTemplateRef ref
      pure $
        DiagnosticReports
          (fromString $ renderTemplateRef ref)
          content
          ( One $
              Diagnostic.emit
                (Diagnostic.Offset $ Temple.getOffset offset)
                Diagnostic.Caret
                (fromString "not in scope")
          )
    ResourceTypeNotFound path field ->
      pure . DiagnosticSimple $
        renderTemplateId resId
          ++ renderTypeProviderPath path
          ++ "missing resource type '"
          ++ Text.unpack field
          ++ "'"
    ResourceNotFound path field ->
      pure . DiagnosticSimple $
        renderTemplateId resId
          ++ renderTypeProviderPath path
          ++ "missing resource '"
          ++ Text.unpack field
          ++ "'"
    PropertyNotFound path field ->
      pure . DiagnosticSimple $
        renderTemplateId resId
          ++ renderTypeProviderPath path
          ++ "missing property '"
          ++ Text.unpack field
          ++ "'"
    TypeError path err' ->
      pure . DiagnosticSimple $
        renderTemplateId resId ++ renderTypeProviderPath path ++ templeTypeErrorMessage err'
  where
    renderTypeProviderPath [] = ""
    renderTypeProviderPath xs@(_ : _) = intercalate "." (fmap Text.unpack xs) ++ ": "

    renderTemplateId templateId = "(" ++ renderResourceId templateId ++ "): "

requireRecord ::
  MonadError TypeProviderError m =>
  -- | Path to type
  [Text] ->
  -- | Type to examine
  Temple.Type ->
  -- | Record fields, record tail
  m ([(Text, Temple.Type)], Maybe Temple.Type)
requireRecord path ty =
  case ty of
    Temple.TRecord fields -> do
      pure $ getRecordFields fields
    _ ->
      throwError $ NotARecord path ty

forRecord ::
  MonadError TypeProviderError m =>
  -- | Path
  [Text] ->
  -- | Record to process
  Temple.Type ->
  {-| How to process each record field

  Arguments:

  * Path
  * Field name
  * Field type
  -}
  ([Text] -> Text -> Temple.Type -> Temple.InferT loc m Temple.Value) ->
  Temple.InferT loc m Temple.Value
forRecord path ty f = do
  (fields, _rest) <- lift $ requireRecord path ty
  fmap (Temple.VRecord . Map.fromList) . for fields $ \(name, ty') -> do
    value <- f (path <> pure name) name ty'
    pure (name, value)

unifyPropertyType ::
  Monad m =>
  -- | Path to type
  [Text] ->
  -- | Expected
  Temple.Type ->
  -- | Actual
  Temple.Type ->
  Temple.InferT () (ExceptT TypeProviderError m) ()
unifyPropertyType path a b = do
  result <- tryError $ Temple.unify () a b
  case result of
    Left err -> lift . throwError $ TypeError path err
    Right x -> pure x

resourceTypeProvider ::
  MonadError DiagnosticReports m =>
  Store m ->
  Store.TransactionId ->
  TypeProvider (WriterT (Set ResourceId) m)
resourceTypeProvider store xactId path resourceTypesTy = do
  forRecord path resourceTypesTy $ \path' resTyName resourcesTy -> do
    mResTy <- lift . lift . lift . Store.lookupResourceType store xactId $ Text.unpack resTyName
    resTy <- maybe (lift . throwError $ ResourceTypeNotFound path resTyName) pure mResTy

    forRecord path' resourcesTy $ \path'' resName propertiesTy -> do
      exists <- lift . lift . lift . Store.doesResourceExist resTy $ Text.unpack resName
      unless exists . lift . throwError $ ResourceNotFound path' resName

      lift . lift . tell . Set.singleton $ ResourceId (Text.unpack resTyName) (Text.unpack resName)

      propertiesTypeProvider
        (Store.hoistResourceType lift resTy)
        resName
        ( \resTy' resName' propName' ->
            runMaybeT $
              MaybeT (defaultPropertyTypeProvider resTy' resName' propName')
                <|> MaybeT (lookupPropertyTypeProvider resTy' resName' propName')
        )
        path''
        propertiesTy

propertiesTypeProvider ::
  MonadError DiagnosticReports m =>
  Store.ResourceType m ->
  -- | Resource name
  Text ->
  (Store.ResourceType m -> Text -> Text -> m (Maybe (TypeProvider m))) ->
  TypeProvider m
propertiesTypeProvider resTy resName propProviders path selfTy = do
  forRecord path selfTy $ \path' propName propTy -> do
    mPropProvider <- lift . lift $ propProviders resTy resName propName
    case mPropProvider of
      Just propProvider ->
        propProvider path' propTy
      Nothing ->
        lift . throwError $ PropertyNotFound path propName

defaultPropertyTypeProvider ::
  MonadError DiagnosticReports m =>
  Store.ResourceType m ->
  -- | Resource name
  Text ->
  -- | Property name
  Text ->
  m (Maybe (TypeProvider m))
defaultPropertyTypeProvider resTy resName propName
  | propName == fromString "metadata" =
      pure . Just $
        \path propTy -> do
          unifyPropertyType path propTy metadataTy

          metadata <- do
            mMetadata <- lift . lift . Store.readResourceMetadata resTy $ Text.unpack resName
            case mMetadata of
              Nothing ->
                pure mempty
              Just content ->
                lift . lift $
                  parseResourceMetadata
                    (Store.resourceTypeConfig resTy)
                    (Store.resourceTypeName resTy)
                    (Text.unpack resName)
                    content
          pure $! Temple.VRecord (fmap metaToTempleValue metadata)
  | propName == fromString "content" =
      pure . Just $
        \path propTy -> do
          let contentTy = Temple.TString

          unifyPropertyType path propTy contentTy

          (mFormat, mContent) <-
            lift . lift $
              (,)
                <$> Store.lookupProperty resTy (Text.unpack resName) "content-format"
                <*> Store.readResource resTy (Text.unpack resName)

          let
            format :: LazyByteString -> LazyByteString
            format =
              case mFormat of
                Just (Toml.VString s) | s == fromString "text" -> id
                Just (Toml.VString s) | s == fromString "line" -> \x -> ByteString.Lazy.Char8.dropWhileEnd (`elem` "\r\n") x
                _ -> id

          case mContent of
            Nothing ->
              pure $ Temple.VString mempty
            Just content ->
              pure . Temple.VString $ format content
  | otherwise =
      pure Nothing
  where
    metadataTy =
      Temple.TRecord $
        foldr
          ( \(metaName, metaCfg) ->
              Temple.TRecordField metaName $
                if metaCfgOptional metaCfg && isNothing (metaCfgDefault metaCfg)
                  then mkOptional . metaToTempleTy $ metaCfgType metaCfg
                  else metaToTempleTy $ metaCfgType metaCfg
          )
          Temple.TRowEnd
          (Map.toList . cfgMetadata $ Store.resourceTypeConfig resTy)

lookupPropertyTypeProvider ::
  MonadError DiagnosticReports m =>
  Store.ResourceType m ->
  -- | Resource name
  Text ->
  -- | Property name
  Text ->
  m (Maybe (TypeProvider m))
lookupPropertyTypeProvider resTy resName propName = do
  mValue <- Store.lookupProperty resTy (Text.unpack resName) (Text.unpack propName)
  case mValue of
    Nothing -> pure Nothing
    Just value -> do
      {- GRIPE: I don't like that I have to generalise and then
      instantiate
      here, but it's the best I can do right now.

      Some unsatisfactory alternatives:

      \* Infer `actualTy` here without generalising and allow any contained metas
        to escape. Somehow ensure that the metas are in scope within the type
        provider.

      \* Move the inference of `actualTy` into the type provider. Doesn't work
        because the type provider works with `Temple.TypeError ()`, but the
        inference of `actualTy` works with `Temple.TypeError Temple.Offset`.
      -}
      (actualValue, actualTyScheme) <- do
        result <-
          Temple.runInferT
            (Temple.emptyInferEnv (const undefined) (Temple.TemplateRef "."))
            Temple.emptyInferState
            ( do
                (value', ty) <- inferTomlValueType value
                pure (value', Temple.generaliseType ty)
            )
        case result of
          Right (_state, ty) -> pure ty
          Left err -> do
            let resId = ResourceId (Store.resourceTypeName resTy) (Text.unpack resName)
            content <- fromJust <$> Store.readProperty resTy (Text.unpack resName) (Text.unpack propName)
            throwError
              . DiagnosticReports
                (fromString $ "(" ++ renderResourceId resId ++ ")")
                content
              =<< templeTypeErrorReport (const undefined) (const undefined) err

      pure . Just $ \path propTy -> do
        actualTy <- Temple.instantiateTypeScheme actualTyScheme
        unifyPropertyType path propTy actualTy

        pure actualValue

inferTomlValueType ::
  Monad m => Toml.TomlValue -> Temple.InferT Temple.Offset m (Temple.Value, Temple.Type)
inferTomlValueType Toml.VTrue =
  pure (Temple.VTrue, Temple.TBool)
inferTomlValueType Toml.VFalse =
  pure (Temple.VFalse, Temple.TBool)
inferTomlValueType (Toml.VString s) =
  pure (Temple.VString . Text.Lazy.Encoding.encodeUtf8 $ LazyText.fromStrict s, Temple.TString)
inferTomlValueType Toml.VInt{} =
  error "TODO: support TOML integers"
inferTomlValueType (Toml.VArray items) = do
  itemTy <- Temple.metavar Temple.KType
  items' <- for items $ \item -> do
    (item', itemTy') <- inferTomlValueType $ Toml.locatedValue item
    Temple.unify (Temple.Offset $ Toml.locatedOffset item) itemTy itemTy'
    pure item'
  pure (Temple.VStream items', Temple.TStream itemTy)
inferTomlValueType (Toml.VRecord fields) = do
  fields' <- for fields $ \(name, value) -> do
    (value', ty) <- inferTomlValueType $ Toml.locatedValue value
    pure (Toml.locatedValue name, (value', ty))
  let !value = Temple.VRecord . Map.fromList $ (fmap . fmap) fst fields'
  pure
    ( value
    , Temple.TRecord $
        foldr
          ( \(name, (_value, ty)) ->
              Temple.TRecordField name ty
          )
          Temple.TRowEnd
          fields'
    )

articlePropertyTypeProvider ::
  MonadError DiagnosticReports m =>
  -- | Previous
  Maybe [(Text, MetadataValue)] ->
  -- | Next
  Maybe [(Text, MetadataValue)] ->
  -- | Rendered article content
  Builder ->
  -- | Property name
  Text ->
  m (Maybe (TypeProvider m))
articlePropertyTypeProvider mPrev mNext content propName
  | propName == fromString "previous" =
      pure . Just $
        \path propTy -> do
          unifyPropertyType path propTy $
            mkOptional (mkRecord [("title", Temple.TString), ("url", Temple.TString)])

          case mPrev of
            Nothing ->
              pure $ Temple.VConstructor (fromString "None") []
            Just prev ->
              -- TODO: guarantee that these values have the correct type
              pure $
                Temple.VConstructor
                  (fromString "Some")
                  [Temple.VRecord . Map.fromList $ (fmap . fmap) metaToTempleValue prev]
  | propName == fromString "next" =
      pure . Just $
        \path propTy -> do
          unifyPropertyType path propTy $
            mkOptional (mkRecord [("title", Temple.TString), ("url", Temple.TString)])

          case mNext of
            Nothing ->
              pure $ Temple.VConstructor (fromString "None") []
            Just next ->
              -- TODO: guarantee that these values have the correct type
              pure $
                Temple.VConstructor
                  (fromString "Some")
                  [Temple.VRecord . Map.fromList $ (fmap . fmap) metaToTempleValue next]
  | propName == fromString "content" =
      pure . Just $
        \_path _propTy -> do
          let _contentTy = Temple.TString

          -- `content` is always a string.
          -- unifyPropertyType path propTy contentTy

          pure . Temple.VString . Text.Lazy.Encoding.encodeUtf8 $ Builder.toLazyText content
  | otherwise =
      pure Nothing

articleHtml ::
  forall m.
  MonadIO m =>
  (Build.ResourceInput m, Build.ResourceInput m, Build.ResourceInput m) ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
articleHtml (iTemplate, iArticle, iAdjacency) oHtml = do
  let inputTemplateRef = Temple.TemplateRef . resourceName $ Build.resourceInputId iTemplate

  let
    templateResourceType = resourceType $ Build.resourceInputId iTemplate

  let
    renderTemplateRef (Temple.TemplateRef name) =
      "(" ++ renderResourceId (ResourceId templateResourceType name) ++ ")"

    readTemplateRef :: Temple.TemplateRef -> Build.ActionT m (Maybe ByteString)
    readTemplateRef (Temple.TemplateRef name) =
      fmap LazyByteString.toStrict <$> Store.readResource (Build.resourceInputType iTemplate) name

  let templateContent = Build.resourceInputContent iTemplate
  template' <-
    case Temple.parse (LazyByteString.toStrict templateContent) of
      Left err ->
        throwError $
          DiagnosticReports
            (fromString $ renderTemplateRef inputTemplateRef)
            templateContent
            (sageErrorReport err)
      Right x -> pure x
  (deps, bindings) <- do
    result <- do
      let ref = Temple.TemplateRef . resourceName $ Build.resourceInputId iTemplate
      runExceptT $ Temple.inferBindings readTemplateRef ref template'
    case result of
      Left err -> do
        let
          getTemplateRef (Temple.TemplateRef name) =
            fromMaybe (error $ "missing resource " ++ renderResourceId (ResourceId templateResourceType name))
              <$> Store.readResource (Build.resourceInputType iTemplate) name

        reports <- templeTypeErrorReport renderTemplateRef getTemplateRef err
        throwError $
          DiagnosticReports
            (fromString $ renderTemplateRef inputTemplateRef)
            (Build.resourceInputContent iTemplate)
            reports
      Right x -> pure x

  html <- do
    let articleContent = Build.resourceInputContent iArticle
    articleContent' <-
      case Text.Lazy.Encoding.decodeUtf8' articleContent of
        Left err -> error $ "TODO: " ++ show err
        Right x -> pure $! LazyText.toStrict x
    markdown <-
      case commonmark ("(" ++ renderResourceId (Build.resourceInputId iArticle) ++ ")") articleContent' of
        Left err -> error $ "TODO: " ++ show err
        Right x -> pure $ unCm (x :: Cm () Blocks)
    Html.runRenderT Html.emptyNotesState . Html.renderBlocks $ Blocks.toList markdown

  (prev, next) <- do
    let adjacencyFile = fromString $ "(" ++ renderResourceId (Build.resourceInputId iAdjacency) ++ ")"
    let adjacencyContent = Build.resourceInputContent iAdjacency
    toml <-
      tomlResult adjacencyFile adjacencyContent . Toml.parse $ LazyByteString.toStrict adjacencyContent
    let
      decoder =
        (,)
          <$> Toml.optionalKey (fromString "previous") (Toml.pstring resourceIdParser)
          <*> Toml.optionalKey (fromString "next") (Toml.pstring resourceIdParser)
    (prev, next) <- tomlResult adjacencyFile adjacencyContent $ Toml.decode toml decoder

    let
      getAdjacencyFields resId@(ResourceId resTyName resName) = do
        store <- Build.askStore
        xactId <- Build.askTransactionId

        resTy <- Store.getResourceType store xactId resTyName
        mContent <- Store.readResourceMetadata resTy resName
        content <- maybe (error $ "resource " ++ renderResourceId resId ++ " has no metadata") pure mContent
        metadata <-
          parseResourceMetadata
            (Store.resourceTypeConfig resTy)
            (Store.resourceTypeName resTy)
            resName
            content
        pure $
          [(fromString "title", prev') | Just prev' <- [Map.lookup (fromString "title") metadata]]
            ++ [(fromString "url", next') | Just next' <- [Map.lookup (fromString "url") metadata]]

    prev' <- traverse getAdjacencyFields prev
    next' <- traverse getAdjacencyFields next

    pure (prev', next')

  bindings' <- for bindings $ \binding -> do
    let name = Temple.bindingName binding
    let tyScheme = Temple.bindingScheme binding

    result <-
      runExceptT $
        case Text.unpack name of
          "resource" -> do
            store <- lift Build.askStore
            xactId <- lift Build.askTransactionId

            (a, _deps) <- do
              let
                f :: (Either e a, w) -> Either e (a, w)
                f (ea, w) = (,w) <$> ea

              mapExceptT (fmap f . runWriterT) $ do
                let readTemplateRef' = lift . lift . readTemplateRef
                let currentTemplate = Temple.TemplateRef "."
                let path' = pure $ Temple.bindingName binding
                result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
                  ty <- Temple.instantiateTypeScheme tyScheme
                  resourceTypeProvider store xactId path' ty
                either (throwError . TypeError path') pure result

            pure a
          "self" -> do
            let readTemplateRef' = lift . readTemplateRef
            let currentTemplate = Temple.TemplateRef "."
            let path' = pure $ Temple.bindingName binding
            result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
              ty <- Temple.instantiateTypeScheme tyScheme
              propertiesTypeProvider
                (Build.resourceInputType iArticle)
                (fromString . resourceName $ Build.resourceInputId iArticle)
                ( \resTy' resName' propName' ->
                    runMaybeT $
                      MaybeT (articlePropertyTypeProvider prev next html propName')
                        <|> MaybeT (defaultPropertyTypeProvider resTy' resName' propName')
                        <|> MaybeT (lookupPropertyTypeProvider resTy' resName' propName')
                )
                path'
                ty
            either (throwError . TypeError path') pure result
          _ ->
            throwError $ uncurry ParameterNotFound (NonEmpty.head $ Temple.bindingLocations binding)
    value <-
      case result of
        Right (_state, value) -> pure value
        Left err -> do
          let
            getTemplateRef (Temple.TemplateRef name') =
              fromMaybe (error $ "missing resource " ++ renderResourceId (ResourceId templateResourceType name'))
                <$> Store.readResource (Build.resourceInputType iTemplate) name'

          throwError
            =<< typeProviderErrorDiagnostic renderTemplateRef getTemplateRef (Build.resourceInputId iTemplate) err

    pure (name, value)

  let
    env = Temple.defaultEvalEnv (Temple.TemplateRef . resourceName $ Build.resourceInputId iTemplate) deps
    output =
      Temple.evalTemplate
        env{Temple.eeScope = Map.fromList bindings' <> Temple.eeScope env}
        template'

  Build.writeResource oHtml () output
