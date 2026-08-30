{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

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
import Blog.Route (RouteEntry (..), renderRouteEntry)
import qualified Blog.Route as Routes
import Blog.Store (Store)
import qualified Blog.Store as Store
import Commonmark.Extensions.Footnote (footnoteSpec)
import Commonmark.Extensions.Wikilinks (TitlePosition (..), wikilinksSpec)
import Commonmark.Pandoc (Cm, unCm)
import Commonmark.Parser (commonmarkWith)
import Commonmark.Syntax (defaultSyntaxSpec)
import Control.Applicative ((<|>))
import Control.Monad (guard, unless)
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
import Data.Functor.Identity (runIdentity)
import Data.List (delete, find, sortOn)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.Monoid (First (..), getFirst)
import Data.Ord (Down (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text.Encoding
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Encoding as Text.Lazy.Encoding
import Data.Time.Clock (UTCTime (..))
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (for)
import qualified Data.Tuple as Tuple
import qualified Temple
import qualified Text.Diagnostic as Diagnostic
import Text.Pandoc.Builder (Block, Blocks)
import qualified Text.Pandoc.Builder as Blocks (toList)
import Text.Pandoc.Definition (Block (..), Inline (..))
import qualified Text.Pandoc.Html as Html
import Text.Pandoc.Walk (query, walk, walkM)
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
      (Build.iResourceAll "article" Build.iAny)
      (Build.oResource "adjacency" $ Build.oMatch "article-" *< Build.oAny)
      articleAdjacency
    <> Build.rule
      "article-dependency"
      (Build.iResource "article" Build.iAny)
      (pure ())
      articleDependency
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
      ( (,)
          <$> Build.iResource "template" (Build.iMatch "post-list.html.temple")
          <*> Build.iAll
            ( (,)
                <$> Build.iResource "article" (Build.iBind "name")
                <*> Build.iResourceOptional "excerpt" (Build.iMatch "article-" <> Build.iBind "name")
            )
      )
      (Build.oResource "html" (Build.oMatch "index"))
      indexHtml
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
      "pdf-route"
      (Build.iResource "pdf" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "pdf-" *< Build.oBind "name"))
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

templateDependency ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m LazyByteString ->
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
      renderResourceId (ResourceId templateResourceType name)

    readTemplateRef :: Temple.TemplateRef -> Build.ActionT m (Maybe ByteString)
    readTemplateRef (Temple.TemplateRef name) =
      fmap LazyByteString.toStrict <$> Store.readResource (Build.resourceInputType iTemplate) name

    getTemplateRef (Temple.TemplateRef name) =
      fromMaybe (error $ "missing resource " ++ renderResourceId (ResourceId templateResourceType name))
        <$> Store.readResource (Build.resourceInputType iTemplate) name

  let ref = Temple.TemplateRef . resourceName $ Build.resourceInputId iTemplate
  (deps, bindings, _template'') <- do
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
            =<< typeProviderErrorDiagnostic renderTemplateRef getTemplateRef (Build.resourceInputId iTemplate) err

  let
    templateDependencies =
      Set.map (\(Temple.TemplateRef r) -> ResourceId "template" r) (Map.keysSet deps)
        <> fold mResources
  Build.setDependencies (Build.resourceInputId iTemplate) templateDependencies

articleAdjacency ::
  Monad m =>
  Build.ResourceInputs m LazyByteString ->
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
            renderResourceId article ++ "'s metadata has an invalid 'published' field: " ++ Text.unpack input'
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
          ++ " expected "
          ++ Temple.renderType ty
          ++ ", got a record"
    ParameterNotFound mOrigin ref offset -> do
      content <- getTemplateRef ref
      pure $
        DiagnosticReports
          (fromString $ "(" ++ renderTemplateRef ref ++ ")")
          content
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
        renderTemplateId resId
          ++ renderTypeProviderPath path
          ++ " missing resource type '"
          ++ Text.unpack field
          ++ "'"
    ResourceNotFound path field ->
      pure . DiagnosticSimple $
        renderTemplateId resId
          ++ renderTypeProviderPath path
          ++ " missing resource '"
          ++ Text.unpack field
          ++ "'"
    PropertyNotFound path field ->
      pure . DiagnosticSimple $
        renderTemplateId resId
          ++ renderTypeProviderPath path
          ++ " missing property '"
          ++ Text.unpack field
          ++ "'"
    TypeError path err' ->
      pure . DiagnosticSimple $
        renderTemplateId resId ++ renderTypeProviderPath path ++ " " ++ templeTypeErrorMessage err'
  where
    renderTypeProviderPath [] = ""
    renderTypeProviderPath [p] = renderPart p
    renderTypeProviderPath (p : ps@(p' : _)) = renderPart p ++ (case p' of PField{} -> "."; PIndex{} -> "") ++ renderTypeProviderPath ps

    renderPart (PField f) = Text.unpack f
    renderPart (PIndex n) = "[" ++ show n ++ "]"

    renderTemplateId templateId = "(" ++ renderResourceId templateId ++ "): "

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
      mMetadata <- Store.readResourceMetadata resTy $ Text.unpack resName
      case mMetadata of
        Nothing ->
          pure mempty
        Just content ->
          parseResourceMetadata
            (Store.resourceTypeConfig resTy)
            (Store.resourceTypeName resTy)
            (Text.unpack resName)
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
                    <$> Store.lookupProperty resTy (Text.unpack resName) "content-format"
                    <*> Store.readResource resTy (Text.unpack resName)

              let
                format :: LazyByteString -> LazyByteString
                format =
                  case mFormat of
                    Just (VString s) | s == fromString "text" -> id
                    Just (VString s) | s == fromString "line" -> \x -> ByteString.Lazy.Char8.dropWhileEnd (`elem` "\r\n") x
                    _ -> id

              case mContent of
                Nothing ->
                  pure $ Temple.CString mempty
                Just content ->
                  pure $ Temple.CString [Temple.CPartText . LazyByteString.toStrict $ format content]
          else
            pure Nothing
    )

lookupPropertyTypeProvider ::
  MonadError DiagnosticReports m =>
  PropertyTypeProvider m
lookupPropertyTypeProvider =
  PropertyTypeProvider $
    \resTy resName propName -> do
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
                    (value', ty) <- inferMetadataValueType value
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
  Builder ->
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
                          Builder.toLazyText content
                      ]
              else
                pure Nothing

loadTemplate ::
  forall m.
  MonadIO m =>
  (Temple.TemplateRef -> Build.ActionT m (Maybe ByteString)) ->
  (Temple.TemplateRef -> String) ->
  Build.ResourceInput m LazyByteString ->
  Build.ActionT
    m
    (Map.Map Temple.TemplateRef Temple.Core, [Temple.Binding], Temple.Core)
loadTemplate readTemplateRef renderTemplateRef iTemplate = do
  let inputTemplateRef = Temple.TemplateRef . resourceName $ Build.resourceInputId iTemplate
  let templateResourceType = resourceType $ Build.resourceInputId iTemplate

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

loadMarkdown ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m LazyByteString ->
  Build.ActionT m (Set ResourceId, [Block])
loadMarkdown input = do
  content <-
    case Text.Lazy.Encoding.decodeUtf8' $ Build.resourceInputContent input of
      Left err -> error $ "TODO: " ++ show err
      Right x -> pure $! LazyText.toStrict x

  let
    result =
      runIdentity $
        commonmarkWith
          (defaultSyntaxSpec <> wikilinksSpec TitleBeforePipe <> footnoteSpec)
          ("(" ++ renderResourceId (Build.resourceInputId input) ++ ")")
          content
  case result of
    Left err ->
      error $ "TODO: " ++ show err
    Right x -> do
      let blocks = Blocks.toList $ unCm (x :: Cm () Blocks)
      (deps, blocks') <- fmap Tuple.swap . runWriterT $ resolveResourceReferences blocks
      pure (deps, removeMetadata blocks')
  where
    wikilinkUrlName =
      "(wikilink in " ++ renderResourceId (Build.resourceInputId input) ++ ")"

    resolveResourceReferences :: [Block] -> WriterT (Set ResourceId) (Build.ActionT m) [Block]
    resolveResourceReferences =
      walkM
        @Inline
        ( \case
            Link (ident, classes, kvs) alts (url, title) | fromString "wikilink" `elem` classes -> do
              let urlInput = Text.Encoding.encodeUtf8 url
              parsed <-
                case Sage.parse Temple.exprParser urlInput of
                  Left err ->
                    throwError $
                      DiagnosticReports
                        (fromString wikilinkUrlName)
                        (LazyByteString.fromStrict urlInput)
                        (sageErrorReport err)
                  Right x -> pure x
              resolved <- resolveResourceReference urlInput parsed
              pure $ Link (ident, delete (fromString "wikilink") classes, kvs) alts (resolved, title)
            x ->
              pure x
        )

    resolveResourceReference ::
      ByteString -> Temple.LExpr Temple.Offset -> WriterT (Set ResourceId) (Build.ActionT m) Text
    resolveResourceReference urlInput expr = do
      let currentTemplateRef = Temple.TemplateRef $ "(" ++ renderResourceId (Build.resourceInputId input) ++ ")"

      let readTemplateRef = const $ error "impossible readTemplateRef"
      let renderTemplateRef = const $ error "impossible renderTemplateRef"
      let getTemplateRef = const $ error "impossible getTemplateRef"

      (deps, bindings, core) <- do
        result <-
          runExceptT $
            Temple.inferBindings readTemplateRef currentTemplateRef (Temple.TemplateBase [Temple.PartExpr expr])
        case result of
          Left err -> do
            throwError
              . DiagnosticReports
                (fromString wikilinkUrlName)
                (LazyByteString.fromStrict urlInput)
              =<< templeTypeErrorReport renderTemplateRef getTemplateRef err
          Right x ->
            pure x

      for_ bindings $ \binding -> do
        unless (Temple.bindingName binding == fromString "resource") $ do
          let (_ref, offset) = NonEmpty.head $ Temple.bindingLocations binding
          throwError $
            DiagnosticReports
              (fromString wikilinkUrlName)
              (LazyByteString.fromStrict urlInput)
              ( One $
                  Diagnostic.emit
                    (Diagnostic.Offset $ Temple.getOffset offset)
                    Diagnostic.Caret
                    (fromString "not in scope")
              )

      case find ((fromString "resource" ==) . Temple.bindingName) bindings of
        Nothing ->
          pure . LazyText.toStrict . Text.Lazy.Encoding.decodeUtf8 . Temple.valueString $
            Temple.evalCore (Temple.defaultEvalEnv deps) core
        Just binding -> do
          store <- lift Build.askStore
          xactId <- lift Build.askTransactionId
          let path = [PField $ Temple.bindingName binding]
          result <- runExceptT $ do
            result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef currentTemplateRef) Temple.emptyInferState $ do
              ty <- Temple.instantiateTypeScheme $ Temple.bindingScheme binding
              resourceTypeProvider store xactId path ty
            either (throwError . TypeError path) pure result

          bindingValue <-
            case result of
              Left err ->
                throwError
                  =<< typeProviderErrorDiagnostic renderTemplateRef getTemplateRef (Build.resourceInputId input) err
              Right (_state, providedCore) ->
                pure providedCore

          let env = Temple.defaultEvalEnv deps
          pure . LazyText.toStrict . Text.Lazy.Encoding.decodeUtf8 . Temple.valueString $
            Temple.evalCore env (Temple.CApp core [(fromString "resource", bindingValue)])

    removeMetadata :: [Block] -> [Block]
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

loadMetadata :: Monad m => Build.ResourceInput m a -> Build.ActionT m (Map Text MetadataValue)
loadMetadata input = do
  let resId = Build.resourceInputId input
  let resTy = Build.resourceInputType input
  let resName = resourceName resId
  mContent <- Store.readResourceMetadata resTy resName
  content <- maybe (error $ "resource " ++ renderResourceId resId ++ " has no metadata") pure mContent
  parseResourceMetadata
    (Store.resourceTypeConfig resTy)
    (Store.resourceTypeName resTy)
    resName
    content

-- TODO: should this be a separate rule? Is there a way to maintain dependencies inside `articleHtml`?
articleDependency ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m LazyByteString ->
  () ->
  Build.ActionT m ()
articleDependency iArticle () = do
  (deps, _markdown) <- loadMarkdown iArticle
  Build.setDependencies (Build.resourceInputId iArticle) deps

articleHtml ::
  forall m.
  MonadIO m =>
  ( Build.ResourceInput m LazyByteString
  , Build.ResourceInput m LazyByteString
  , Build.ResourceInput m LazyByteString
  ) ->
  (Build.ResourceOutput m (), Build.ResourceOutput m ()) ->
  Build.ActionT m ()
articleHtml (iTemplate, iArticle, iAdjacency) (oExcerpt, oHtml) = do
  let templateResourceType = resourceType $ Build.resourceInputId iTemplate

  let
    readTemplateRef :: Temple.TemplateRef -> Build.ActionT m (Maybe ByteString)
    readTemplateRef (Temple.TemplateRef name) =
      fmap LazyByteString.toStrict <$> Store.readResource (Build.resourceInputType iTemplate) name

  let
    renderTemplateRef (Temple.TemplateRef name) =
      renderResourceId (ResourceId templateResourceType name)

  (deps, bindings, template'') <- loadTemplate readTemplateRef renderTemplateRef iTemplate

  (mExcerpt, html) <- do
    (_deps, document) <- loadMarkdown iArticle

    let
      isComment (RawBlock format value) = format == fromString "html" && fromString "<!--" `Text.isPrefixOf` value
      isComment _ = False

      mExcerpt = getFirst $ query @Block (\block -> First $ block <$ guard (not $ isComment block)) document

    (,)
      <$> traverse (Html.runRenderT Html.emptyNotesState . Html.renderBlock) mExcerpt
      <*> Html.runRenderT Html.emptyNotesState (Html.renderBlocks document)

  for_ mExcerpt $ Build.writeResource oExcerpt () . Text.Lazy.Encoding.encodeUtf8 . Builder.toLazyText

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
                let path' = pure . PField $ Temple.bindingName binding
                result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
                  ty <- Temple.instantiateTypeScheme tyScheme
                  resourceTypeProvider store xactId path' ty
                either (throwError . TypeError path') pure result

            pure a
          "self" -> do
            let readTemplateRef' = lift . readTemplateRef
            let currentTemplate = Temple.TemplateRef "."
            let path' = pure . PField $ Temple.bindingName binding
            result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
              ty <- Temple.instantiateTypeScheme tyScheme
              propertiesTypeProvider
                (Build.resourceInputType iArticle)
                (fromString . resourceName $ Build.resourceInputId iArticle)
                ( articlePropertyTypeProvider prev next html
                    <> nestedPropertyTypeProvider (fromString "metadata") metadataPropertyTypeProvider
                    <> contentPropertyTypeProvider
                    <> lookupPropertyTypeProvider
                )
                path'
                ty
            either (throwError . TypeError path') pure result
          _ -> do
            let (bindingRef, bindingOffset) = NonEmpty.head $ Temple.bindingLocations binding
            let inputTemplateRef = Temple.TemplateRef . resourceName $ Build.resourceInputId iTemplate
            throwError $
              ParameterNotFound
                (inputTemplateRef <$ guard (bindingRef /= inputTemplateRef))
                bindingRef
                bindingOffset
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

  let output = Temple.evalTemplate (Temple.defaultEvalEnv deps) template'' bindings'

  Build.writeResource oHtml () output

  metadata <- loadMetadata iArticle
  let mUrl = Map.lookup (fromString "url") metadata
  for_ mUrl $ Build.setResourceProperty oHtml () "url"

pageDependency ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m LazyByteString ->
  () ->
  Build.ActionT m ()
pageDependency iPage () = do
  (deps, _markdown) <- loadMarkdown iPage
  Build.setDependencies (Build.resourceInputId iPage) deps

pageHtml ::
  forall m.
  MonadIO m =>
  ( Build.ResourceInput m LazyByteString
  , Build.ResourceInput m LazyByteString
  ) ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
pageHtml (iTemplate, iPage) oHtml = do
  let inputTemplateRef = Temple.TemplateRef . resourceName $ Build.resourceInputId iTemplate

  let
    templateResourceType = resourceType $ Build.resourceInputId iTemplate

  let
    renderTemplateRef (Temple.TemplateRef name) =
      renderResourceId (ResourceId templateResourceType name)

    readTemplateRef :: Temple.TemplateRef -> Build.ActionT m (Maybe ByteString)
    readTemplateRef (Temple.TemplateRef name) =
      fmap LazyByteString.toStrict <$> Store.readResource (Build.resourceInputType iTemplate) name

  (deps, bindings, template'') <- loadTemplate readTemplateRef renderTemplateRef iTemplate

  html <- do
    (_deps, markdown) <- loadMarkdown iPage
    Html.runRenderT Html.emptyNotesState (Html.renderBlocks markdown)

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
                let path' = pure . PField $ Temple.bindingName binding
                result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
                  ty <- Temple.instantiateTypeScheme tyScheme
                  resourceTypeProvider store xactId path' ty
                either (throwError . TypeError path') pure result

            pure a
          "self" -> do
            let readTemplateRef' = lift . readTemplateRef
            let currentTemplate = Temple.TemplateRef "."
            let path' = pure . PField $ Temple.bindingName binding
            result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
              ty <- Temple.instantiateTypeScheme tyScheme
              propertiesTypeProvider
                (Build.resourceInputType iPage)
                (fromString . resourceName $ Build.resourceInputId iPage)
                ( nestedPropertyTypeProvider (fromString "metadata") metadataPropertyTypeProvider
                    <> constantPropertyTypeProvider
                      (fromString "content")
                      ( Temple.CString
                          [ Temple.CPartText . LazyByteString.toStrict . Text.Lazy.Encoding.encodeUtf8 $ Builder.toLazyText html
                          ]
                      , Temple.TString
                      )
                    <> lookupPropertyTypeProvider
                )
                path'
                ty
            either (throwError . TypeError path') pure result
          _ -> do
            let (bindingRef, bindingOffset) = NonEmpty.head $ Temple.bindingLocations binding
            throwError $
              ParameterNotFound
                (inputTemplateRef <$ guard (bindingRef /= inputTemplateRef))
                bindingRef
                bindingOffset
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

  let output = Temple.evalTemplate (Temple.defaultEvalEnv deps) template'' bindings'

  Build.writeResource oHtml () output

  metadata <- loadMetadata iPage
  let mUrl = Map.lookup (fromString "url") metadata
  for_ mUrl $ Build.setResourceProperty oHtml () "url"

indexHtml ::
  forall m.
  MonadIO m =>
  ( Build.ResourceInput m LazyByteString
  , [(Build.ResourceInput m LazyByteString, Maybe (Build.ResourceInput m LazyByteString))]
  ) ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
indexHtml (iTemplate, iArticlesWithExcerpts) oHtml = do
  let
    inputTemplateRef = Temple.TemplateRef . resourceName $ Build.resourceInputId iTemplate

    templateResourceType = resourceType $ Build.resourceInputId iTemplate

    renderTemplateRef (Temple.TemplateRef name) =
      renderResourceId (ResourceId templateResourceType name)

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

  (deps, bindings, template'') <- do
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
                let path' = pure . PField $ Temple.bindingName binding
                result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
                  ty <- Temple.instantiateTypeScheme tyScheme
                  resourceTypeProvider store xactId path' ty
                either (throwError . TypeError path') (pure . snd) result

            pure a
          "self" -> do
            let readTemplateRef' = lift . readTemplateRef
            let currentTemplate = Temple.TemplateRef "."
            let path' = pure . PField $ Temple.bindingName binding
            result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
              ty <- Temple.instantiateTypeScheme tyScheme
              propertiesTypeProvider
                (Build.resourceInputType iTemplate)
                (fromString . resourceName $ Build.resourceInputId iTemplate)
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
                path'
                ty
            either (throwError . TypeError path') (pure . snd) result
          "tag" -> do
            let readTemplateRef' = lift . readTemplateRef
            let currentTemplate = Temple.TemplateRef "."
            let path' = pure . PField $ Temple.bindingName binding
            result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
              ty <- Temple.instantiateTypeScheme tyScheme

              let actualTy = mkOptional Temple.TString
              unifyPropertyType path' ty actualTy

              pure $ Temple.CConstructor (fromString "None") []
            either (throwError . TypeError path') (pure . snd) result
          "posts" -> do
            let readTemplateRef' = lift . readTemplateRef
            let currentTemplate = Temple.TemplateRef "."
            let path' = pure . PField $ Temple.bindingName binding
            sortedArticles <-
              fmap snd . sortOn (Down . fst)
                <$> for
                  (zip [0 ..] iArticlesWithExcerpts)
                  ( \(ix, (iArticle, miExcerpt)) -> do
                      let mPublished = Map.lookup (fromString "published") (Build.resourceInputMetadata iArticle)
                      published <-
                        case mPublished of
                          Nothing ->
                            throwError $
                              PropertyNotFound
                                (path' <> pure (PIndex ix) <> pure (PField $ fromString "metadata"))
                                (fromString "published")
                          Just published
                            | VString s <- published -> pure s
                            | otherwise -> error "TODO: published not a string"
                      pure (published, (iArticle, miExcerpt))
                  )

            result <- Temple.runInferT (Temple.emptyInferEnv readTemplateRef' currentTemplate) Temple.emptyInferState $ do
              ty <- Temple.instantiateTypeScheme tyScheme
              listTypeProvider
                ( fmap
                    ( \(iArticle, miExcerpt) ->
                        propertiesTypeProvider
                          (Build.resourceInputType iArticle)
                          (fromString . resourceName $ Build.resourceInputId iArticle)
                          -- TODO: this property should come from metadata.
                          --
                          -- Currently blocked on having a good syntax for sum types in metadata.
                          ( nestedPropertyTypeProvider (fromString "metadata") $
                              constantPropertyTypeProvider
                                (fromString "type")
                                ( Temple.CConstructor (fromString "Article") []
                                , Temple.TSum $
                                    foldr
                                      (uncurry Temple.TSumConstructor)
                                      Temple.TRowEnd
                                      [ (fromString "Article", [])
                                      ,
                                        ( fromString "Reply"
                                        ,
                                          [ Temple.TRecord $
                                              foldr
                                                (uncurry Temple.TRecordField)
                                                Temple.TRowEnd
                                                [
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
                                )
                                <> foldMap
                                  ( \iExcerpt ->
                                      constantPropertyTypeProvider
                                        (fromString "excerpt")
                                        ( Temple.CString [Temple.CPartText . LazyByteString.toStrict $ Build.resourceInputContent iExcerpt]
                                        , Temple.TString
                                        )
                                  )
                                  miExcerpt
                                <> metadataPropertyTypeProvider
                          )
                    )
                    sortedArticles
                )
                path'
                ty
            either (throwError . TypeError path') (pure . snd) result
          _ -> do
            let (bindingRef, bindingOffset) = NonEmpty.head $ Temple.bindingLocations binding
            throwError $
              ParameterNotFound
                (inputTemplateRef <$ guard (bindingRef /= inputTemplateRef))
                bindingRef
                bindingOffset
    value <-
      case result of
        Right value -> pure value
        Left err -> do
          let
            getTemplateRef (Temple.TemplateRef name') =
              fromMaybe (error $ "missing resource " ++ renderResourceId (ResourceId templateResourceType name'))
                <$> Store.readResource (Build.resourceInputType iTemplate) name'

          throwError
            =<< typeProviderErrorDiagnostic renderTemplateRef getTemplateRef (Build.resourceInputId iTemplate) err

    pure (name, value)

  let output = Temple.evalTemplate (Temple.defaultEvalEnv deps) template'' bindings'

  Build.writeResource oHtml () output

  Build.setResourceProperty oHtml () "url" $ VString (fromString "/")

resourceRoute ::
  forall m.
  MonadIO m =>
  Build.ResourceInput m LazyByteString ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
resourceRoute iContent oRoute = do
  let resId = Build.resourceInputId iContent
  let resTy = Build.resourceInputType iContent
  let resName = resourceName resId
  mUrl <- Store.lookupProperty resTy resName "url"
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
