{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Blog.Rules (rules) where

import Blog
  ( MetadataType (..)
  , MetadataValue (..)
  , Name
  , ResourceId (..)
  , cfgMetadata
  , metaCfgDefault
  , metaCfgOptional
  , metaCfgType
  , metadataValueString
  , propertyParser
  , renderName
  , renderResourceId
  , resourceIdParser
  , resourceNameParser
  , resourceTypeParser
  , unsafeName
  )
import Blog.Build ((*<))
import qualified Blog.Build as Build
import Blog.Diagnostic (DiagnosticReports (..), Reports (..))
import Blog.Error (sageErrorReport, templeTypeErrorMessage, templeTypeErrorReport, tomlResult)
import Blog.Metadata (parseResourceMetadata)
import Blog.Pandoc (htmlWriterOptions, markdownReaderOptions)
import Blog.Route (RouteEntry (..), renderRouteEntry)
import qualified Blog.Route as Routes
import Blog.Store (Store)
import qualified Blog.Store as Store
import Control.Applicative (many, (<|>))
import Control.Monad (guard, unless, (<=<))
import Control.Monad.Error.Class (MonadError, throwError, tryError)
import Control.Monad.Except (ExceptT (..), mapExceptT, runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..), runMaybeT)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT, tell)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (fold, for_)
import Data.List (find, sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Monoid (All (..), First (..), getAll, getFirst)
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (for)
import qualified Data.Tuple as Tuple
import qualified Temple
import qualified Text.Diagnostic as Diagnostic
import Text.Pandoc (PandocPure)
import qualified Text.Pandoc as Pandoc
import Text.Pandoc.Builder (Block)
import qualified Text.Pandoc.Builder as Pandoc
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc, nullAttr)
import Text.Pandoc.Walk (Walkable, query, walk, walkM)
import qualified Text.Sage as Sage
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
      ( (,)
          <$> Build.iResourceAll "article" Build.iAny
          <*> Build.iResourceAll "note" Build.iAny
      )
      (Build.oResource "adjacency" Build.oAny)
      articleAdjacency
    <> Build.rule
      "article-dependency"
      (Build.iResource "article" Build.iAny)
      (pure ())
      markdownDependency
    <> Build.rule
      "article-html"
      ( (,,)
          <$> Build.iResource "template" (Build.iMatch "article.html.temple")
          <*> Build.iResource "article" (Build.iBind "name")
          <*> Build.iResource "adjacency" (Build.iMatch "article-" <> Build.iBind "name")
      )
      ( (,)
          <$> Build.oResource "excerpt" (Build.oMatch "article-" *< Build.oBind "name")
          <*> Build.oResource "html" (Build.oMatch "article-" *< Build.oBind "name")
      )
      articleHtml
    <> Build.rule
      "note-dependency"
      (Build.iResource "note" Build.iAny)
      (pure ())
      markdownDependency
    <> Build.rule
      "note-html"
      ( (,,)
          <$> Build.iResource "template" (Build.iMatch "note.html.temple")
          <*> Build.iResource "note" (Build.iBind "name")
          <*> Build.iResource "adjacency" (Build.iMatch "note-" <> Build.iBind "name")
      )
      (Build.oResource "html" (Build.oMatch "note-" *< Build.oBind "name"))
      noteHtml
    <> Build.rule
      "page-dependency"
      (Build.iResource "page" Build.iAny)
      (pure ())
      pageDependency
    <> Build.rule
      "page-html"
      ( (,)
          <$> Build.iResource "template" (Build.iMatch "page.html.temple")
          <*> Build.iResource "page" (Build.iBind "name")
      )
      (Build.oResource "html" (Build.oMatch "page-" *< Build.oBind "name"))
      pageHtml
    <> Build.rule
      "index-html"
      ( (,,)
          <$> Build.iResource "template" (Build.iMatch "post-list.html.temple")
          <*> Build.iAll
            ( (,)
                <$> Build.iResource "article" (Build.iBind "name")
                <*> Build.iResourceOptional "excerpt" (Build.iMatch "article-" <> Build.iBind "name")
            )
          <*> Build.iAll (Build.iResource "note" Build.iAny)
      )
      (Build.oResource "html" (Build.oMatch "index"))
      indexHtml
    -- TODO: there should be some catch-all logic for routing via the URL property.
    <> Build.rule
      "html-route"
      (Build.iResource "html" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "html-" *< Build.oBind "name"))
      resourceRoute
    <> Build.rule
      "css-route"
      (Build.iResource "css" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "css-" *< Build.oBind "name"))
      resourceRoute
    <> Build.rule
      "js-route"
      (Build.iResource "js" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "js-" *< Build.oBind "name"))
      resourceRoute
    <> Build.rule
      "pdf-route"
      (Build.iResource "pdf" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "pdf-" *< Build.oBind "name"))
      resourceRoute
    <> Build.rule
      "woff2-route"
      (Build.iResource "woff2" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "woff2-" *< Build.oBind "name"))
      resourceRoute
    <> Build.rule
      "gif-route"
      (Build.iResource "gif" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "gif-" *< Build.oBind "name"))
      resourceRoute
    <> Build.rule
      "png-route"
      (Build.iResource "png" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "png-" *< Build.oBind "name"))
      resourceRoute
    <> Build.rule
      "svg-route"
      (Build.iResource "svg" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "svg-" *< Build.oBind "name"))
      resourceRoute

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

loadTemplate ::
  forall m.
  MonadIO m =>
  (Temple.TemplateRef -> Build.ActionT m (Maybe ByteString)) ->
  (Temple.TemplateRef -> String) ->
  Build.ResourceInput m ByteString ->
  Build.ActionT
    m
    (Map.Map Temple.TemplateRef Temple.Core, [Temple.Binding], Temple.Core)
loadTemplate readTemplateRef renderTemplateRef iTemplate = do
  let inputTemplateRef = Temple.TemplateRef . renderName . resourceName $ Build.resourceInputId iTemplate
  let templateResourceType = resourceType $ Build.resourceInputId iTemplate

  let templateContent = Build.resourceInputContent iTemplate
  template' <- parseTemplate (renderTemplateRef inputTemplateRef) templateContent

  let
    getTemplateRef (Temple.TemplateRef name) =
      fromMaybe
        (error $ "missing resource " ++ renderResourceId (ResourceId templateResourceType $ unsafeName name))
        <$> Store.readResource (Build.resourceInputType iTemplate) (unsafeName name)

  let ref = Temple.TemplateRef . renderName . resourceName $ Build.resourceInputId iTemplate
  inferBindings
    readTemplateRef
    renderTemplateRef
    getTemplateRef
    ( "(" ++ renderResourceId (Build.resourceInputId iTemplate) ++ ")"
    , Build.resourceInputContent iTemplate
    )
    ref
    template'

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
  result <- runExceptT $ Temple.inferBindings readTemplateRef inputRef template
  case result of
    Right x -> pure x
    Left err -> do
      throwError
        =<< DiagnosticReports
          (fromString location)
          (LazyByteString.fromStrict input)
          <$> templeTypeErrorReport renderTemplateRef getTemplateRef err

templateDependency ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m ByteString ->
  () ->
  Build.ActionT m ()
templateDependency iTemplate () = do
  let
    templateResourceType = resourceType $ Build.resourceInputId iTemplate

  let
    renderTemplateRef (Temple.TemplateRef name) =
      renderResourceId (ResourceId templateResourceType (unsafeName name))

    readTemplateRef :: Temple.TemplateRef -> Build.ActionT m (Maybe ByteString)
    readTemplateRef (Temple.TemplateRef name) =
      Store.readResource (Build.resourceInputType iTemplate) (unsafeName name)

    getTemplateRef (Temple.TemplateRef name) =
      fromMaybe
        (error $ "missing resource " ++ renderResourceId (ResourceId templateResourceType (unsafeName name)))
        <$> Store.readResource (Build.resourceInputType iTemplate) (unsafeName name)

  (deps, bindings, _template'') <- loadTemplate readTemplateRef renderTemplateRef iTemplate

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
        let path = pure . PField $ Temple.bindingName binding
        let readTemplateRef' = lift . lift . readTemplateRef
        result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' bindingRef) Temple.emptyInferState $ do
          ty <- Temple.instantiateTypeScheme $ Temple.bindingScheme binding
          resourceTypeProvider store xactId path ty
        either (throwError . TypeError path) pure result
      case result of
        Right (_state, _value) -> pure resources
        Left err ->
          throwError
            =<< typeProviderErrorDiagnostic
              renderTemplateRef
              getTemplateRef
              (renderResourceId $ Build.resourceInputId iTemplate)
              err

  let
    templateDependencies =
      Set.map
        (\(Temple.TemplateRef r) -> ResourceId (unsafeName "template") (unsafeName r))
        (Map.keysSet deps)
        <> fold mResources
  Build.setDependencies (Build.resourceInputId iTemplate) templateDependencies

articleAdjacency ::
  MonadIO m =>
  (Build.ResourceInputs m ByteString, Build.ResourceInputs m ByteString) ->
  Build.ResourceOutput m String ->
  Build.ActionT m ()
articleAdjacency (iArticles, iNotes) oAdjacency = do
  store <- Build.askStore
  xactId <- Build.askTransactionId

  articlesWithPublished <- getResourcesWithPublished store xactId iArticles
  notesWithPublished <- getResourcesWithPublished store xactId iNotes

  let sortedArticles = fmap snd . sortOn fst $ articlesWithPublished ++ notesWithPublished
  let adjacencies = makeAdjacencies sortedArticles

  for_ adjacencies $ \adjacency -> do
    let Adjacency prev (ResourceId currentResTyName currentResName) next = adjacency
    let
      -- TODO: string escaping, move to `tomlin` library
      tomlString = (fromString "\"" <>) . (<> fromString "\"")
    Build.writeResource
      oAdjacency
      (renderName currentResTyName ++ "-" ++ renderName currentResName)
      ( foldMap (<> fromString "\n") $
          [ fromString "previous = " <> tomlString (fromString $ renderResourceId resId) | Just resId <- [prev]
          ]
            ++ [fromString "next = " <> tomlString (fromString $ renderResourceId resId) | Just resId <- [next]]
      )
  where
    getResourcesWithPublished ::
      MonadIO m =>
      Store (Build.ActionT m) ->
      Store.TransactionId ->
      Build.ResourceInputs m a ->
      Build.ActionT m [(UTCTime, ResourceId)]
    getResourcesWithPublished store xactId inputs = do
      resTy <- do
        let resTyName = Build.resourceInputsType inputs
        Store.getResourceType store xactId resTyName

      inputs' <- Store.listResource resTy
      for inputs' $ \input -> do
        metadata <- do
          mMetadata <- Store.readProperty resTy (resourceName input) (unsafeName "metadata")
          case mMetadata of
            Nothing ->
              throwError . DiagnosticSimple $ renderResourceId input ++ " has no metadata"
            Just metadata ->
              parseResourceMetadata
                (Store.resourceTypeConfig resTy)
                (Store.resourceTypeName resTy)
                (resourceName input)
                metadata

        published <- do
          published <- case Map.lookup (fromString "published") metadata of
            Nothing ->
              throwError . DiagnosticSimple $ renderResourceId input ++ "'s metadata has no 'published' field"
            Just x ->
              pure x
          let !published' = metadataValueString published
          let
            parseDateTime :: Text -> Maybe UTCTime
            parseDateTime = iso8601ParseM . Text.unpack

            parseDate :: Text -> Maybe UTCTime
            parseDate x = do
              day <- iso8601ParseM $ Text.unpack x
              pure $ UTCTime day 0
          case parseDateTime published' <|> parseDate published' of
            Nothing ->
              throwError . DiagnosticSimple $
                renderResourceId input ++ "'s metadata has an invalid 'published' field: " ++ Text.unpack published'
            Just x -> pure (x :: UTCTime)
        pure (published, input)

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
metaToTempleTy (TRecord fs) =
  Temple.TRecord $
    foldr (\(field, ty) -> Temple.TRecordField field (metaToTempleTy ty)) Temple.TRowEnd fs

metaToTempleCore :: MetadataValue -> Temple.Core
metaToTempleCore (VString s) =
  Temple.CString [Temple.CPartText $ Text.Encoding.encodeUtf8 s]
metaToTempleCore VTrue =
  Temple.CTrue
metaToTempleCore VFalse =
  Temple.CFalse
metaToTempleCore (VList xs) =
  Temple.CArray (fmap metaToTempleCore xs)
metaToTempleCore (VRecord fields) =
  Temple.CRecord ((fmap . fmap) metaToTempleCore fields)
metaToTempleCore (VConstructor name args) =
  Temple.CConstructor name (fmap metaToTempleCore args)

data Part
  = PField Text
  | PIndex Int

type TypeProvider m =
  [Part] -> Temple.Type -> Temple.InferT () (ExceptT TypeProviderError m) Temple.Core

data TypeProviderError
  = NotARecord
      ![Part]
      -- | Expected type
      !Temple.Type
  | ParameterNotFound
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

typeProviderErrorDiagnostic ::
  MonadIO m =>
  (Temple.TemplateRef -> String) ->
  (Temple.TemplateRef -> m ByteString) ->
  -- | Error location name
  String ->
  TypeProviderError ->
  m DiagnosticReports
typeProviderErrorDiagnostic renderTemplateRef getTemplateRef location err =
  -- TODO: point to the location in the template?
  case err of
    NotARecord path ty ->
      pure . DiagnosticSimple $
        location
          ++ ": "
          ++ renderTypeProviderPath path
          ++ ": expected "
          ++ Temple.renderType ty
          ++ ", got a record"
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
    ResourceTypeNotFound path field ->
      pure . DiagnosticSimple $
        location
          ++ ": "
          ++ renderTypeProviderPath path
          ++ ": missing resource type '"
          ++ Text.unpack field
          ++ "'"
    ResourceNotFound path field ->
      pure . DiagnosticSimple $
        location
          ++ ": "
          ++ renderTypeProviderPath path
          ++ ": missing resource '"
          ++ Text.unpack field
          ++ "'"
    PropertyNotFound path field ->
      pure . DiagnosticSimple $
        location
          ++ ": "
          ++ renderTypeProviderPath path
          ++ ": missing property '"
          ++ Text.unpack field
          ++ "'"
    TypeError path err' ->
      pure . DiagnosticSimple $
        location ++ ": " ++ renderTypeProviderPath path ++ ": " ++ templeTypeErrorMessage err'
  where
    renderTypeProviderPath [] = ""
    renderTypeProviderPath [p] = renderPart p
    renderTypeProviderPath (p : ps@(p' : _)) = renderPart p ++ (case p' of PField{} -> "."; PIndex{} -> "") ++ renderTypeProviderPath ps

    renderPart (PField f)
      | Text.elem '.' f = "`" ++ Text.unpack f ++ "`"
      | otherwise = Text.unpack f
    renderPart (PIndex n) = "[" ++ show n ++ "]"

requireRecord ::
  MonadError TypeProviderError m =>
  -- | Path to type
  [Part] ->
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
  [Part] ->
  -- | Record to process
  Temple.Type ->
  {-| How to process each record field

  Arguments:

  * Path
  * Field name
  * Field type
  -}
  ([Part] -> Text -> Temple.Type -> Temple.InferT loc m Temple.Core) ->
  Temple.InferT loc m Temple.Core
forRecord path ty f = do
  (fields, _rest) <- lift $ requireRecord path ty
  fmap Temple.CRecord . for fields $ \(name, ty') -> do
    value <- f (path <> pure (PField name)) name ty'
    pure (name, value)

unifyPropertyType ::
  Monad m =>
  -- | Path to type
  [Part] ->
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

textToName :: Text -> Name
textToName = unsafeName . Text.unpack

nameToText :: Name -> Text
nameToText = fromString . renderName

resourceTypeProvider ::
  MonadError DiagnosticReports m =>
  Store m ->
  Store.TransactionId ->
  TypeProvider (WriterT (Set ResourceId) m)
resourceTypeProvider store xactId path resourceTypesTy = do
  forRecord path resourceTypesTy $ \path' resTyName resourcesTy -> do
    mResTy <- lift . lift . lift . Store.lookupResourceType store xactId $ textToName resTyName
    resTy <- maybe (lift . throwError $ ResourceTypeNotFound path resTyName) pure mResTy

    forRecord path' resourcesTy $ \path'' resName propertiesTy -> do
      exists <- lift . lift . lift . Store.doesResourceExist resTy $ textToName resName
      unless exists . lift . throwError $ ResourceNotFound path' resName

      lift . lift . tell . Set.singleton $ ResourceId (textToName resTyName) (textToName resName)

      propertiesTypeProvider
        (Store.hoistResourceType lift resTy)
        resName
        ( nestedPropertyTypeProvider (fromString "metadata") metadataPropertyTypeProvider
            <> contentPropertyTypeProvider
            <> lookupPropertyTypeProvider
        )
        path''
        propertiesTy

propertiesTypeProvider ::
  MonadError DiagnosticReports m =>
  Store.ResourceType m ->
  -- | Resource name
  Text ->
  PropertyTypeProvider m ->
  TypeProvider m
propertiesTypeProvider resTy resName provider path selfTy = do
  forRecord path selfTy $ \path' propName propTy -> do
    mPropProvider <- lift . lift $ runPropertyTypeProvider provider resTy resName propName
    case mPropProvider of
      Just propProvider ->
        propProvider path' propTy
      Nothing ->
        lift . throwError $ PropertyNotFound path propName

newtype PropertyTypeProvider m
  = PropertyTypeProvider
  { runPropertyTypeProvider ::
      Store.ResourceType m ->
      -- \| Resource name
      Text ->
      -- \| Property name
      Text ->
      m (Maybe (TypeProvider m))
  }

instance Monad m => Semigroup (PropertyTypeProvider m) where
  PropertyTypeProvider a <> PropertyTypeProvider b =
    PropertyTypeProvider $
      \resTy resName propName ->
        runMaybeT $
          MaybeT (a resTy resName propName)
            <|> MaybeT (b resTy resName propName)

instance Monad m => Monoid (PropertyTypeProvider m) where
  mempty = PropertyTypeProvider $ \_resTy _resName _propName -> pure Nothing

nestedPropertyTypeProvider ::
  MonadError DiagnosticReports m =>
  Text ->
  PropertyTypeProvider m ->
  PropertyTypeProvider m
nestedPropertyTypeProvider propName provider =
  PropertyTypeProvider $
    \resTy resName propName' ->
      if propName' == propName
        then pure . Just $ propertiesTypeProvider resTy resName provider
        else pure Nothing

constantPropertyTypeProvider ::
  MonadError DiagnosticReports m =>
  Text ->
  (Temple.Core, Temple.Type) ->
  PropertyTypeProvider m
constantPropertyTypeProvider propName (value, valueTy) =
  PropertyTypeProvider $
    \_resTy _resName propName' ->
      if propName' == propName
        then pure . Just $ \path propTy -> value <$ unifyPropertyType path propTy valueTy
        else pure Nothing

metadataPropertyTypeProvider :: MonadError DiagnosticReports m => PropertyTypeProvider m
metadataPropertyTypeProvider =
  PropertyTypeProvider $ \resTy resName propName -> do
    -- TODO: this will parse out metadata for every property. It should be parsed only once.
    metadata <- do
      mMetadata <- Store.readProperty resTy (textToName resName) (unsafeName "metadata")
      case mMetadata of
        Nothing ->
          pure mempty
        Just content ->
          parseResourceMetadata
            (Store.resourceTypeConfig resTy)
            (Store.resourceTypeName resTy)
            (textToName resName)
            content

    case Map.lookup propName . cfgMetadata $ Store.resourceTypeConfig resTy of
      Nothing ->
        pure Nothing
      Just metaCfg ->
        pure . Just $
          \path propTy -> do
            let
              metadataType =
                -- TODO: this should be moved out so that it's in sync with
                -- the result of `parseResourceMetadata`.
                if metaCfgOptional metaCfg && isNothing (metaCfgDefault metaCfg)
                  then mkOptional . metaToTempleTy $ metaCfgType metaCfg
                  else metaToTempleTy $ metaCfgType metaCfg
              metadataValue = metaToTempleCore . fromJust $ Map.lookup propName metadata

            unifyPropertyType path propTy metadataType

            pure metadataValue

contentPropertyTypeProvider :: MonadError DiagnosticReports m => PropertyTypeProvider m
contentPropertyTypeProvider =
  PropertyTypeProvider
    ( \resTy resName propName ->
        if propName == fromString "content"
          then pure . Just $
            \path propTy -> do
              let contentTy = Temple.TString

              unifyPropertyType path propTy contentTy

              (mFormat, mContent) <-
                lift . lift $
                  (,)
                    <$> Store.lookupProperty resTy (textToName resName) (unsafeName "content-format")
                    <*> Store.readResource resTy (textToName resName)

              let
                format :: ByteString -> ByteString
                format =
                  case mFormat of
                    Just (VString s) | s == fromString "text" -> id
                    Just (VString s) | s == fromString "line" -> \x -> ByteString.Char8.dropWhileEnd (`elem` "\r\n") x
                    _ -> id

              case mContent of
                Nothing ->
                  pure $ Temple.CString mempty
                Just content ->
                  pure $ Temple.CString [Temple.CPartText $ format content]
          else
            pure Nothing
    )

lookupPropertyTypeProvider ::
  MonadError DiagnosticReports m =>
  PropertyTypeProvider m
lookupPropertyTypeProvider =
  PropertyTypeProvider $
    \resTy resName propName -> do
      mValue <- Store.lookupProperty resTy (textToName resName) (textToName propName)
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
                    (value', ty) <- inferMetadataValueType value
                    pure (value', Temple.generaliseType ty)
                )
            case result of
              Right (_state, ty) -> pure ty
              Left err -> do
                let resId = ResourceId (Store.resourceTypeName resTy) (textToName resName)
                content <- fromJust <$> Store.readProperty resTy (textToName resName) (textToName propName)
                throwError
                  . DiagnosticReports
                    (fromString $ "(" ++ renderResourceId resId ++ ")")
                    (LazyByteString.fromStrict content)
                  =<< templeTypeErrorReport (const undefined) (const undefined) err

          pure . Just $ \path propTy -> do
            actualTy <- Temple.instantiateTypeScheme actualTyScheme
            unifyPropertyType path propTy actualTy

            pure actualValue

listTypeProvider ::
  MonadError DiagnosticReports m =>
  [TypeProvider m] ->
  TypeProvider m
listTypeProvider items path ty = do
  itemTy <- Temple.metavar Temple.KType
  unifyPropertyType path ty (Temple.TStream itemTy)
  itemTy' <- Temple.zonkNoDefault itemTy
  items' <- for (zip [0 ..] items) $ \(ix, item) -> do
    item (path <> pure (PIndex ix)) itemTy'
  pure $ Temple.CArray items'

inferMetadataValueType ::
  Monad m => MetadataValue -> Temple.InferT Temple.Offset m (Temple.Core, Temple.Type)
inferMetadataValueType VTrue =
  pure (Temple.CTrue, Temple.TBool)
inferMetadataValueType VFalse =
  pure (Temple.CFalse, Temple.TBool)
inferMetadataValueType (VString s) =
  pure (Temple.CString [Temple.CPartText $ Text.Encoding.encodeUtf8 s], Temple.TString)
inferMetadataValueType (VRecord fields) = do
  rest <- Temple.metavar Temple.KRow
  (fields', fieldTys) <-
    unzip . fmap (\(f, (a, b)) -> ((f, a), (f, b)))
      <$> (traverse . traverse) inferMetadataValueType fields
  pure (Temple.CRecord fields', Temple.TRecord $ foldr (uncurry Temple.TRecordField) rest fieldTys)
inferMetadataValueType (VConstructor name args) = do
  rest <- Temple.metavar Temple.KRow
  (args', argTys) <- unzip <$> traverse inferMetadataValueType args
  pure (Temple.CConstructor name args', Temple.TSum (Temple.TSumConstructor name argTys rest))
inferMetadataValueType (VList items) = do
  itemTy <- Temple.metavar Temple.KType
  items' <- for items $ \item -> do
    (item', itemTy') <- inferMetadataValueType item
    -- TODO: the fact that I have to write `Offset 0` means that something's wrong.
    Temple.unify (Temple.Offset 0) itemTy itemTy'
    pure item'
  pure (Temple.CArray items', Temple.TStream itemTy)

articlePropertyTypeProvider ::
  MonadError DiagnosticReports m =>
  -- | Previous
  Maybe [(Text, MetadataValue)] ->
  -- | Next
  Maybe [(Text, MetadataValue)] ->
  -- | Rendered article content
  Text ->
  PropertyTypeProvider m
articlePropertyTypeProvider mPrev mNext content =
  PropertyTypeProvider $ \_resTy _resName propName ->
    if propName == fromString "previous"
      then pure . Just $
        \path propTy -> do
          unifyPropertyType path propTy $
            mkOptional (mkRecord [("title", Temple.TString), ("url", Temple.TString)])

          case mPrev of
            Nothing ->
              pure $ Temple.CConstructor (fromString "None") []
            Just prev ->
              -- TODO: guarantee that these values have the correct type
              pure $
                Temple.CConstructor
                  (fromString "Some")
                  [Temple.CRecord $ (fmap . fmap) metaToTempleCore prev]
      else
        if propName == fromString "next"
          then pure . Just $
            \path propTy -> do
              unifyPropertyType path propTy $
                mkOptional (mkRecord [("title", Temple.TString), ("url", Temple.TString)])

              case mNext of
                Nothing ->
                  pure $ Temple.CConstructor (fromString "None") []
                Just next ->
                  -- TODO: guarantee that these values have the correct type
                  pure $
                    Temple.CConstructor
                      (fromString "Some")
                      [Temple.CRecord $ (fmap . fmap) metaToTempleCore next]
          else
            if propName == fromString "content"
              then pure . Just $
                \_path _propTy -> do
                  let _contentTy = Temple.TString

                  -- `content` is always a string.
                  -- unifyPropertyType path propTy contentTy

                  pure $
                    Temple.CString
                      [ Temple.CPartText . LazyByteString.toStrict . Text.Lazy.Encoding.encodeUtf8 $
                          LazyText.fromStrict content
                      ]
              else
                pure Nothing

linkHeaders :: Walkable Block a => a -> a
linkHeaders =
  walk @Block
    ( \block ->
        case block of
          Header level (ident, classes, attrs) content
            | level > 1
            , not (Text.null ident) ->
                let
                  ignore = fromString "link_headers:ignore"
                  linked =
                    Header
                      level
                      (ident, classes, filter (\(key, _value) -> key /= ignore) attrs)
                      [Link nullAttr content (fromString "#" <> ident, mempty)]
                  unlinked = Header level (ident, classes, filter (\(key, _value) -> key /= ignore) attrs) content
                in
                  case Text.unpack <$> lookup ignore attrs of
                    Nothing -> linked
                    Just "true" -> unlinked
                    Just "false" -> linked
                    Just value -> error $ "invalid link_headers:ignore value: " <> value
          _ -> block
    )

tableOfContents :: Walkable Block a => a -> a
tableOfContents document =
  walk @Block
    ( \block ->
        case block of
          Div attr@(ident, _classes, _attributes) children
            | ident == fromString "toc"
            , all ignorable children ->
                Div
                  attr
                  -- This breaks if I use `Pandoc.Header 3 nullAttr [Str "Contents"]`
                  -- I don't know why.
                  [ RawBlock (fromString "html") (fromString "<h3>Contents</h3>")
                  , contents
                  ]
          _ -> block
    )
    document
  where
    ignorable :: Block -> Bool
    ignorable =
      getAll
        . query
          ( \case
              RawBlock format html ->
                All $
                  format == fromString "html"
                    && isJust (Text.stripPrefix (fromString "<!--") html)
              _ ->
                All False
          )

    contents =
      fromTocHeaders . toTocHeaders $
        query @Block
          ( \block -> case block of
              Header{} -> [block]
              _ -> mempty
          )
          document

data TocHeader = TocHeader
  { tocHeaderLevel :: Int
  , tocHeaderId :: Text
  , tocHeaderClasses :: [Text]
  , tocHeaderAttrs :: [(Text, Text)]
  , tocHeaderContent :: [Inline]
  , tocHeaderChildren :: [TocHeader]
  }
  deriving (Eq, Show)

toTocHeaders :: [Block] -> [TocHeader]
toTocHeaders = go 2
  where
    go :: Int -> [Block] -> [TocHeader]
    go level =
      fmap
        ( \((headerLevel, (identifier, classes, attrs), content), blocks) ->
            let omitChildren =
                  case lookup (fromString "toc:omit_children") attrs of
                    Just value ->
                      case Text.unpack value of
                        "true" ->
                          True
                        "false" ->
                          False
                        _ ->
                          error $ "invalid toc:omit_children value: " <> Text.unpack value
                    Nothing ->
                      False
            in TocHeader
                 headerLevel
                 identifier
                 classes
                 attrs
                 content
                 (if omitChildren then [] else go (level + 1) blocks)
        )
        . snd
        . separateBy
          ( \case
              Header headerLevel attrs content
                | level == headerLevel ->
                    Just (headerLevel, attrs, content)
              _ -> Nothing
          )

data BreakOn a b
  = Found {prefix :: [a], target :: b, suffix :: [a]}
  | Missing [a]

breakOn :: (a -> Maybe b) -> [a] -> BreakOn a b
breakOn predicate items =
  case items of
    [] ->
      Missing []
    item : items' ->
      case predicate item of
        Nothing ->
          case breakOn predicate items' of
            Found{prefix, target, suffix} ->
              Found{prefix = item : prefix, target, suffix}
            Missing items'' ->
              Missing (item : items'')
        Just item' ->
          Found{prefix = [], target = item', suffix = items'}

separateBy :: (a -> Maybe b) -> [a] -> ([a], [(b, [a])])
separateBy predicate items =
  case breakOn predicate items of
    Missing items' ->
      (items', [])
    Found{prefix, target, suffix} ->
      case separateBy predicate suffix of
        (prefix', suffix') ->
          (prefix, (target, prefix') : suffix')

fromTocHeaders :: [TocHeader] -> Block
fromTocHeaders contents =
  BulletList $
    ( \content ->
        Plain [Link nullAttr (tocHeaderContent content) (fromString "#" <> tocHeaderId content, mempty)]
          : case tocHeaderChildren content of
            [] -> []
            _ ->
              [fromTocHeaders $ tocHeaderChildren content]
    )
      <$> contents

loadMarkdown ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m ByteString ->
  Build.ActionT m (Set ResourceId, Pandoc)
loadMarkdown input = do
  content <-
    case Text.Encoding.decodeUtf8' $ Build.resourceInputContent input of
      Left err -> error $ "TODO: " ++ show err
      Right x -> pure x

  let result = Pandoc.runPure $ Pandoc.readMarkdown markdownReaderOptions content
  case result of
    Left err ->
      error $ "TODO: " ++ show err
    Right document -> do
      (deps, document') <- fmap Tuple.swap . runWriterT $ resolveResourceReferences document
      let document'' = tableOfContents . linkHeaders $ removeMetadata document'
      pure (deps, document'')
  where
    resourceUriParser :: Sage.Parser (Temple.Core, Temple.Type)
    resourceUriParser =
      ( \resTyName resName propertyPath ->
          let
            core =
              foldl
                (\acc field -> Temple.CField acc field)
                (Temple.CVar $ fromString "resource")
                (resTyName : resName : propertyPath)

            ty =
              foldr
                ( \field rest ->
                    Temple.TRecord $ Temple.TRecordField field rest Temple.TRowEnd
                )
                Temple.TString
                (resTyName : resName : propertyPath)
          in
            (core, ty)
      )
        <$ Sage.string (fromString "resource")
        <* Sage.char ':'
        <*> fmap nameToText resourceTypeParser
        <* Sage.char ':'
        <*> fmap nameToText resourceNameParser
        <*> many (Sage.char '/' *> fmap nameToText propertyParser)

    resolveResourceReferences :: Pandoc -> WriterT (Set ResourceId) (Build.ActionT m) Pandoc
    resolveResourceReferences =
      walkM
        @Inline
        ( \case
            Link (ident, classes, kvs) alts (url, title)
              | let urlInput = Text.Encoding.encodeUtf8 url
              , Right parsed <- Sage.parse (resourceUriParser <* Sage.eof) urlInput -> do
                  resolved <- resolveResourceReference parsed
                  pure $ Link (ident, classes, kvs) alts (resolved, title)
            Image (ident, classes, kvs) alts (url, title)
              | let urlInput = Text.Encoding.encodeUtf8 url
              , Right parsed <- Sage.parse (resourceUriParser <* Sage.eof) urlInput -> do
                  resolved <- resolveResourceReference parsed
                  pure $ Image (ident, classes, kvs) alts (resolved, title)
            RawInline format content | format == fromString "html" -> do
              content' <- resolveResourceReferencesRaw content
              pure $ RawInline format content'
            x ->
              pure x
        )
        <=< walkM
          @Block
          ( \case
              RawBlock format content | format == fromString "html" -> do
                content' <- resolveResourceReferencesRaw content
                pure $ RawBlock format content'
              x ->
                pure x
          )

    resolveResourceReference ::
      (Temple.Core, Temple.Type) -> WriterT (Set ResourceId) (Build.ActionT m) Text
    resolveResourceReference (core, ty) = do
      let currentTemplateRef = Temple.TemplateRef $ "(" ++ renderResourceId (Build.resourceInputId input) ++ ")"

      let readTemplateRef = const $ error "impossible readTemplateRef"
      let renderTemplateRef = const $ error "impossible renderTemplateRef"
      let getTemplateRef = const $ error "impossible getTemplateRef"

      store <- lift Build.askStore
      xactId <- lift Build.askTransactionId
      result <- runExceptT $ do
        let path = [PField $ fromString "resource"]
        result <-
          Temple.runInferT (Temple.emptyInferEnv readTemplateRef currentTemplateRef) Temple.emptyInferState $
            resourceTypeProvider store xactId path ty
        either (throwError . TypeError path) pure result

      let env = Temple.defaultEvalEnv mempty
      bindingValue <-
        case result of
          Left err ->
            throwError
              =<< typeProviderErrorDiagnostic
                renderTemplateRef
                getTemplateRef
                (renderResourceId $ Build.resourceInputId input)
                err
          Right (_state, providedCore) ->
            pure $ Temple.evalCore env providedCore

      let env' = env{Temple.eeScope = Map.singleton (fromString "resource") bindingValue}
      pure . LazyText.toStrict . Text.Lazy.Encoding.decodeUtf8 . Temple.valueString $
        Temple.evalCore env' core

    resolveResourceReferencesRaw :: Text -> WriterT (Set ResourceId) (Build.ActionT m) Text
    resolveResourceReferencesRaw content = do
      let
        readTemplateRef = const $ error "impossible readTemplateRef"
        renderTemplateRef = const $ error "impossible renderTemplateRef"
        getTemplateRef = const $ error "impossible getTemplateRef"

      let location = renderResourceId (Build.resourceInputId input) ++ ", HTML fragment"
      let content' = Text.Encoding.encodeUtf8 content
      template <- parseTemplate ("(" ++ location ++ ")") content'

      let inputRef = Temple.TemplateRef "."
      -- TODO: register these deps
      (deps, bindings, template') <-
        lift $
          inferBindings
            readTemplateRef
            renderTemplateRef
            getTemplateRef
            ("(" ++ renderResourceId (Build.resourceInputId input) ++ ", inline HTML)", content')
            inputRef
            template

      bindings' <- for bindings $ \binding -> do
        let name = Temple.bindingName binding

        (resIds, value) <-
          lift . handleTypeProvider renderTemplateRef getTemplateRef location $
            case Text.unpack name of
              "resource" -> makeResourceBinding readTemplateRef binding
              _ -> bindingParameterNotFound inputRef binding

        tell resIds

        pure (name, value)

      pure
        . LazyText.toStrict
        . Text.Lazy.Encoding.decodeUtf8
        $ Temple.evalTemplate (Temple.defaultEvalEnv deps) template' bindings'

    removeMetadata :: Pandoc -> Pandoc
    removeMetadata =
      walk
        @[Block]
        ( filter $
            \case
              CodeBlock (_ident, classes, _kvs) _content ->
                not $ fromString "toml+blog-metadata" `elem` classes
              _ ->
                True
        )

renderHtml :: MonadError DiagnosticReports m => Pandoc -> m Text
renderHtml = pandoc . Pandoc.writeHtml5String htmlWriterOptions

loadMetadata :: Monad m => Build.ResourceInput m a -> Build.ActionT m (Map Text MetadataValue)
loadMetadata input = do
  let resId = Build.resourceInputId input
  let resTy = Build.resourceInputType input
  let resName = resourceName resId
  mContent <- Store.readProperty resTy resName (unsafeName "metadata")
  content <- maybe (error $ "resource " ++ renderResourceId resId ++ " has no metadata") pure mContent
  parseResourceMetadata
    (Store.resourceTypeConfig resTy)
    (Store.resourceTypeName resTy)
    resName
    content

-- TODO: should this be a separate rule? Is there a way to maintain dependencies inside `articleHtml`?
markdownDependency ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m ByteString ->
  () ->
  Build.ActionT m ()
markdownDependency input () = do
  (deps, _markdown) <- loadMarkdown input
  Build.setDependencies (Build.resourceInputId input) deps

pandoc :: MonadError DiagnosticReports m => PandocPure a -> m a
pandoc = either (throwError . DiagnosticSimple . Text.unpack . Pandoc.renderError) pure . Pandoc.runPure

getAdjacency ::
  MonadIO m =>
  Build.ResourceInput m ByteString ->
  Build.ActionT m (Maybe [(Text, MetadataValue)], Maybe [(Text, MetadataValue)])
getAdjacency iAdjacency = do
  let adjacencyFile = fromString $ "(" ++ renderResourceId (Build.resourceInputId iAdjacency) ++ ")"
  let adjacencyContent = Build.resourceInputContent iAdjacency
  toml <-
    tomlResult adjacencyFile adjacencyContent $ Toml.parse adjacencyContent
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
      mContent <- Store.readProperty resTy resName (unsafeName "metadata")
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

makeResourceBinding ::
  Monad m =>
  (Temple.TemplateRef -> Build.ActionT m (Maybe ByteString)) ->
  Temple.Binding ->
  ExceptT TypeProviderError (Build.ActionT m) (Set ResourceId, Temple.Core)
makeResourceBinding readTemplateRef binding = do
  store <- lift Build.askStore
  xactId <- lift Build.askTransactionId

  ((_state, a), deps) <- do
    let
      f :: (Either e a, w) -> Either e (a, w)
      f (ea, w) = (,w) <$> ea

    mapExceptT (fmap f . runWriterT) $ do
      let readTemplateRef' = lift . lift . readTemplateRef
      let currentTemplate = Temple.TemplateRef "."
      let path' = pure . PField $ Temple.bindingName binding
      result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
        ty <- Temple.instantiateTypeScheme $ Temple.bindingScheme binding
        resourceTypeProvider store xactId path' ty
      either (throwError . TypeError path') pure result

  pure (deps, a)

bindingParameterNotFound ::
  MonadError TypeProviderError m =>
  -- | Location (for error reporting)
  Temple.TemplateRef ->
  Temple.Binding ->
  m a
bindingParameterNotFound inputRef binding = do
  let (bindingRef, bindingOffset) = NonEmpty.head $ Temple.bindingLocations binding
  throwError $
    ParameterNotFound
      (inputRef <$ guard (bindingRef /= inputRef))
      bindingRef
      bindingOffset

handleTypeProvider ::
  MonadIO m =>
  (Temple.TemplateRef -> String) ->
  (Temple.TemplateRef -> Build.ActionT m ByteString) ->
  -- | Location name (for error reporting)
  String ->
  ExceptT TypeProviderError (Build.ActionT m) a ->
  Build.ActionT m a
handleTypeProvider renderTemplateRef getTemplateRef location ma = do
  ea <- runExceptT ma
  case ea of
    Right a -> pure a
    Left err -> do
      throwError
        =<< typeProviderErrorDiagnostic renderTemplateRef getTemplateRef location err

renderTemplate ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m ByteString ->
  Map Text (BindingTypeProvider (Build.ActionT m)) ->
  Build.ActionT m LazyByteString
renderTemplate iTemplate typeProviders = do
  let templateResourceType = resourceType $ Build.resourceInputId iTemplate

  let
    readTemplateRef :: Temple.TemplateRef -> Build.ActionT m (Maybe ByteString)
    readTemplateRef (Temple.TemplateRef name) =
      Store.readResource (Build.resourceInputType iTemplate) (unsafeName name)

  let
    renderTemplateRef (Temple.TemplateRef name) =
      renderResourceId (ResourceId templateResourceType (unsafeName name))

  let
    getTemplateRef (Temple.TemplateRef name) =
      fromMaybe
        (error $ "missing resource " ++ renderResourceId (ResourceId templateResourceType (unsafeName name)))
        <$> Store.readResource (Build.resourceInputType iTemplate) (unsafeName name)

  (deps, bindings, template'') <- loadTemplate readTemplateRef renderTemplateRef iTemplate

  bindings' <- for bindings $ \binding -> do
    let name = Temple.bindingName binding

    value <-
      handleTypeProvider
        renderTemplateRef
        getTemplateRef
        (renderResourceId $ Build.resourceInputId iTemplate)
        $ case Map.lookup name typeProviders of
          Just typeProvider -> typeProvider readTemplateRef binding
          Nothing ->
            bindingParameterNotFound
              (Temple.TemplateRef . renderName . resourceName $ Build.resourceInputId iTemplate)
              binding

    pure (name, value)

  pure $ Temple.evalTemplate (Temple.defaultEvalEnv deps) template'' bindings'

type BindingTypeProvider m =
  (Temple.TemplateRef -> m (Maybe ByteString)) ->
  Temple.Binding ->
  ExceptT TypeProviderError m Temple.Core

resourceBindingTypeProvider :: Monad m => BindingTypeProvider (Build.ActionT m)
resourceBindingTypeProvider readTemplateRef binding = do
  (_resIds, core) <- makeResourceBinding readTemplateRef binding
  pure core

bindingTypeProvider :: Monad m => TypeProvider m -> BindingTypeProvider m
bindingTypeProvider provider readTemplateRef binding = do
  let readTemplateRef' = lift . readTemplateRef
  let currentTemplate = Temple.TemplateRef "."
  let path' = pure . PField $ Temple.bindingName binding
  result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
    ty <- Temple.instantiateTypeScheme $ Temple.bindingScheme binding
    provider path' ty
  either (throwError . TypeError path') (pure . snd) result

articleHtml ::
  forall m.
  MonadIO m =>
  ( Build.ResourceInput m ByteString
  , Build.ResourceInput m ByteString
  , Build.ResourceInput m ByteString
  ) ->
  (Build.ResourceOutput m (), Build.ResourceOutput m ()) ->
  Build.ActionT m ()
articleHtml (iTemplate, iArticle, iAdjacency) (oExcerpt, oHtml) = do
  (mExcerpt, html) <- do
    (_deps, document) <- loadMarkdown iArticle

    (,)
      <$> ( do
              let mMetadataExcerpt = Map.lookup (fromString "excerpt") $ Build.resourceInputMetadata iArticle
              case mMetadataExcerpt of
                Just (VString metadataExcerpt) | not $ Text.null metadataExcerpt -> do
                  pure . Just $ metadataExcerpt
                _ -> do
                  let
                    mExcerpt =
                      getFirst $
                        query @Block
                          ( \case
                              block@Para{} ->
                                First . Just $
                                  walk @[Inline] (filter (\case Note{} -> False; _ -> True)) block
                              _ -> mempty
                          )
                          document
                  traverse
                    (pandoc . Pandoc.writeHtml5String htmlWriterOptions . Pandoc.doc . Pandoc.singleton)
                    mExcerpt
          )
      <*> renderHtml document

  for_ mExcerpt $
    Build.writeResource oExcerpt () . Text.Lazy.Encoding.encodeUtf8 . LazyText.fromStrict

  (prev, next) <- getAdjacency iAdjacency

  output <-
    renderTemplate iTemplate $
      Map.fromList
        [ (fromString "resource", resourceBindingTypeProvider)
        ,
          ( fromString "self"
          , bindingTypeProvider $
              propertiesTypeProvider
                (Build.resourceInputType iArticle)
                (nameToText . resourceName $ Build.resourceInputId iArticle)
                ( articlePropertyTypeProvider prev next html
                    <> nestedPropertyTypeProvider (fromString "metadata") metadataPropertyTypeProvider
                    <> contentPropertyTypeProvider
                    <> lookupPropertyTypeProvider
                )
          )
        ]

  Build.writeResource oHtml () output

  metadata <- loadMetadata iArticle
  let mUrl = Map.lookup (fromString "url") metadata
  for_ mUrl $ Build.setResourceProperty oHtml () (unsafeName "url")

noteHtml ::
  forall m.
  MonadIO m =>
  ( Build.ResourceInput m ByteString
  , Build.ResourceInput m ByteString
  , Build.ResourceInput m ByteString
  ) ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
noteHtml (iTemplate, iNote, iAdjacency) oHtml = do
  html <- do
    (_deps, document) <- loadMarkdown iNote
    pandoc (Pandoc.writeHtml5String htmlWriterOptions document)

  (prev, next) <- getAdjacency iAdjacency

  output <-
    renderTemplate iTemplate $
      Map.fromList
        [ (fromString "resource", resourceBindingTypeProvider)
        ,
          ( fromString "self"
          , bindingTypeProvider $
              propertiesTypeProvider
                (Build.resourceInputType iNote)
                (nameToText . resourceName $ Build.resourceInputId iNote)
                ( articlePropertyTypeProvider prev next html
                    <> nestedPropertyTypeProvider (fromString "metadata") metadataPropertyTypeProvider
                    <> contentPropertyTypeProvider
                    <> lookupPropertyTypeProvider
                )
          )
        ]

  Build.writeResource oHtml () output

  metadata <- loadMetadata iNote
  let mUrl = Map.lookup (fromString "url") metadata
  for_ mUrl $ Build.setResourceProperty oHtml () (unsafeName "url")

pageDependency ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m ByteString ->
  () ->
  Build.ActionT m ()
pageDependency iPage () = do
  (deps, _markdown) <- loadMarkdown iPage
  Build.setDependencies (Build.resourceInputId iPage) deps

pageHtml ::
  forall m.
  MonadIO m =>
  ( Build.ResourceInput m ByteString
  , Build.ResourceInput m ByteString
  ) ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
pageHtml (iTemplate, iPage) oHtml = do
  html <- do
    (_deps, markdown) <- loadMarkdown iPage
    pandoc $ Pandoc.writeHtml5String htmlWriterOptions markdown

  output <-
    renderTemplate iTemplate $
      Map.fromList
        [ (fromString "resource", resourceBindingTypeProvider)
        ,
          ( fromString "self"
          , bindingTypeProvider $
              propertiesTypeProvider
                (Build.resourceInputType iPage)
                (nameToText . resourceName $ Build.resourceInputId iPage)
                ( nestedPropertyTypeProvider (fromString "metadata") metadataPropertyTypeProvider
                    <> constantPropertyTypeProvider
                      (fromString "content")
                      ( Temple.CString
                          [ Temple.CPartText . LazyByteString.toStrict . Text.Lazy.Encoding.encodeUtf8 $
                              LazyText.fromStrict html
                          ]
                      , Temple.TString
                      )
                    <> lookupPropertyTypeProvider
                )
          )
        ]

  Build.writeResource oHtml () output

  metadata <- loadMetadata iPage
  let mUrl = Map.lookup (fromString "url") metadata
  for_ mUrl $ Build.setResourceProperty oHtml () (unsafeName "url")

data IndexItem m
  = IndexArticle
      -- | Article
      (Build.ResourceInput m ByteString)
      -- | Excerpt
      (Maybe (Build.ResourceInput m ByteString))
  | IndexNote
      -- | Note
      (Build.ResourceInput m ByteString)
      -- | Rendered HTML
      Text

indexHtml ::
  MonadIO m =>
  ( Build.ResourceInput m ByteString
  , [(Build.ResourceInput m ByteString, Maybe (Build.ResourceInput m ByteString))]
  , [Build.ResourceInput m ByteString]
  ) ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
indexHtml (iTemplate, iArticlesWithExcerpts, iNotes) oHtml = do
  output <-
    renderTemplate iTemplate $
      Map.fromList
        [ (fromString "resource", resourceBindingTypeProvider)
        ,
          ( fromString "self"
          , bindingTypeProvider $
              propertiesTypeProvider
                (Build.resourceInputType iTemplate)
                (nameToText . resourceName $ Build.resourceInputId iTemplate)
                ( nestedPropertyTypeProvider
                    (fromString "metadata")
                    ( constantPropertyTypeProvider
                        (fromString "url")
                        (Temple.CString [Temple.CPartText $ fromString "/"], Temple.TString)
                        <> constantPropertyTypeProvider
                          (fromString "title")
                          (Temple.CString [Temple.CPartText $ fromString "blog.ielliott.io"], Temple.TString)
                        <> constantPropertyTypeProvider
                          (fromString "description")
                          (Temple.CString [Temple.CPartText $ fromString "Isaac Elliott's personal blog."], Temple.TString)
                        <> constantPropertyTypeProvider (fromString "math") (Temple.CFalse, Temple.TBool)
                        <> constantPropertyTypeProvider (fromString "chinese") (Temple.CFalse, Temple.TBool)
                        <> constantPropertyTypeProvider (fromString "asciinema") (Temple.CFalse, Temple.TBool)
                    )
                    <> nestedPropertyTypeProvider (fromString "metadata") metadataPropertyTypeProvider
                    <> contentPropertyTypeProvider
                    <> lookupPropertyTypeProvider
                )
          )
        ,
          ( fromString "tag"
          , \readTemplateRef binding -> do
              let readTemplateRef' = lift . readTemplateRef
              let currentTemplate = Temple.TemplateRef "."
              let path' = pure . PField $ Temple.bindingName binding
              result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
                ty <- Temple.instantiateTypeScheme $ Temple.bindingScheme binding

                let actualTy = mkOptional Temple.TString
                unifyPropertyType path' ty actualTy

                pure $ Temple.CConstructor (fromString "None") []
              either (throwError . TypeError path') (pure . snd) result
          )
        ,
          ( fromString "posts"
          , bindingTypeProvider $ \path ty -> do
              let
                getPublished ix input = do
                  let mPublished = Map.lookup (fromString "published") (Build.resourceInputMetadata input)
                  case mPublished of
                    Nothing ->
                      throwError $
                        PropertyNotFound
                          (path <> pure (PIndex ix) <> pure (PField $ fromString "metadata"))
                          (fromString "published")
                    Just published
                      | VString s <- published -> pure s
                      | otherwise -> error "TODO: published not a string"

              articlesWithExcerptsWithPublished <-
                for
                  (zip [0 ..] iArticlesWithExcerpts)
                  ( \(ix, (iArticle, miExcerpt)) -> do
                      published <- lift $ getPublished ix iArticle
                      pure (published, IndexArticle iArticle miExcerpt)
                  )

              notesWithPublished <-
                for
                  (zip [0 ..] iNotes)
                  ( \(ix, iNote) -> do
                      published <- lift $ getPublished ix iNote
                      (_deps, document) <- lift . lift $ loadMarkdown iNote
                      html <- lift . lift $ renderHtml document
                      pure (published, IndexNote iNote html)
                  )

              let sortedPosts = fmap snd . sortOn (Down . fst) $ articlesWithExcerptsWithPublished ++ notesWithPublished

              let
                postTypeTy =
                  Temple.TSum $
                    foldr
                      (uncurry Temple.TSumConstructor)
                      Temple.TRowEnd
                      [
                        ( fromString "Article"
                        ,
                          [ Temple.TRecord $
                              foldr
                                (uncurry Temple.TRecordField)
                                Temple.TRowEnd
                                [
                                  ( fromString "excerpt"
                                  , Temple.TString
                                  )
                                ]
                          ]
                        )
                      ,
                        ( fromString "Note"
                        ,
                          [ Temple.TRecord $
                              foldr
                                (uncurry Temple.TRecordField)
                                Temple.TRowEnd
                                [
                                  ( fromString "content"
                                  , Temple.TString
                                  )
                                ,
                                  ( fromString "references"
                                  , Temple.TStream $
                                      Temple.TRecord $
                                        foldr
                                          (uncurry Temple.TRecordField)
                                          Temple.TRowEnd
                                          [ (fromString "url", Temple.TString)
                                          , (fromString "title", Temple.TString)
                                          ]
                                  )
                                ]
                          ]
                        )
                      ]
              listTypeProvider
                ( fmap
                    ( \case
                        IndexArticle iArticle miExcerpt ->
                          propertiesTypeProvider
                            (Build.resourceInputType iArticle)
                            (nameToText . resourceName $ Build.resourceInputId iArticle)
                            -- TODO: this property should come from metadata.
                            --
                            -- Currently blocked on having a good syntax for sum types in metadata.
                            ( nestedPropertyTypeProvider (fromString "metadata") $
                                constantPropertyTypeProvider
                                  (fromString "type")
                                  ( Temple.CConstructor
                                      (fromString "Article")
                                      [ Temple.CRecord
                                          [ ( fromString "excerpt"
                                            , Temple.CString [Temple.CPartText $ Build.resourceInputContent iExcerpt]
                                            )
                                          | Just iExcerpt <- [miExcerpt]
                                          ]
                                      ]
                                  , postTypeTy
                                  )
                                  <> metadataPropertyTypeProvider
                            )
                        IndexNote iNote html ->
                          propertiesTypeProvider
                            (Build.resourceInputType iNote)
                            (nameToText . resourceName $ Build.resourceInputId iNote)
                            -- TODO: this property should come from metadata.
                            --
                            -- Currently blocked on having a good syntax for sum types in metadata.
                            ( nestedPropertyTypeProvider (fromString "metadata") $
                                constantPropertyTypeProvider
                                  (fromString "type")
                                  ( Temple.CConstructor
                                      (fromString "Note")
                                      [ Temple.CRecord
                                          [ (fromString "content", Temple.CString [Temple.CPartText $ Text.Encoding.encodeUtf8 html])
                                          ,
                                            ( fromString "references"
                                            , metaToTempleCore . fromJust $
                                                Map.lookup (fromString "references") (Build.resourceInputMetadata iNote)
                                            )
                                          ]
                                      ]
                                  , postTypeTy
                                  )
                                  <> metadataPropertyTypeProvider
                            )
                    )
                    sortedPosts
                )
                path
                ty
          )
        ]

  Build.writeResource oHtml () output

  Build.setResourceProperty oHtml () (unsafeName "url") $ VString (fromString "/")

resourceRoute ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m ByteString ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
resourceRoute iContent oRoute = do
  let resId = Build.resourceInputId iContent
  let resTy = Build.resourceInputType iContent
  let resName = resourceName resId
  mUrl <- Store.lookupProperty resTy resName (unsafeName "url")
  for_ mUrl $ \url -> do
    -- GRIPE: should we really have to parse out `path`, only to immediately print it via `renderRouteEntry`?
    path <-
      case url of
        VString s -> do
          let input = Text.Encoding.encodeUtf8 s
          case Sage.parse (Routes.pathParser <* Sage.eof) input of
            Right path ->
              pure path
            Left err ->
              throwError $
                DiagnosticReports
                  (fromString $ "(" ++ renderResourceId resId ++ ":metadata:url)")
                  (LazyByteString.fromStrict input)
                  (sageErrorReport err)
        _ ->
          throwError . DiagnosticSimple $ "(" ++ renderResourceId resId ++ ":metadata:url): not a string"
    Build.writeResource oRoute () $
      renderRouteEntry (RouteEntry path (Build.resourceInputId iContent))
