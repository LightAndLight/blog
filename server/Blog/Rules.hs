{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Blog.Rules (rules) where

import Blog
  ( MetadataValue (..)
  , Name
  , ResourceId (..)
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
import Blog.Diagnostic (DiagnosticReports (..))
import Blog.Error (sageErrorReport, tomlResult)
import Blog.Metadata (MetadataValueDecoder, parseResourceMetadata, runMetadataValueDecoder)
import qualified Blog.Metadata as Metadata
import Blog.Pandoc (htmlWriterOptions, markdownReaderOptions, pandoc)
import Blog.Route (RouteEntry (..), renderRouteEntry)
import qualified Blog.Route as Routes
import qualified Blog.Store as Store
import Blog.Template
  ( Fields (..)
  , ProviderError (..)
  , ProviderT
  , Value (..)
  , boolValue
  , bytestringValue
  , fields
  , inferBindings
  , loadTemplate
  , optionalValue
  , parseTemplate
  , provideBinding
  , provideTypeScheme
  , recordValue
  , renderTemplate
  , runProviderT
  , stringValue
  , textValue
  )
import Blog.Time (renderUTCTime)
import Control.Applicative (many, (<|>))
import Control.Monad (unless, (<=<))
import Control.Monad.Error.Class (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (WriterT, runWriterT)
import Control.Monad.Writer.Class (tell)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString.Char8
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Foldable (fold, for_)
import Data.Functor ((<&>))
import Data.List (find, sortOn)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
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
import Data.Time.Clock (UTCTime (..), getCurrentTime)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import Data.Traversable (for)
import qualified Data.Tuple as Tuple
import qualified Temple
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
    <> Build.rule
      "index-feed"
      ( (,,,)
          <$> Build.iResource "feed-config" (Build.iMatch "index")
          <*> Build.iResource "template" (Build.iMatch "feed.xml.temple")
          <*> Build.iAll
            ( (,)
                <$> Build.iResource "article" (Build.iBind "name")
                <*> Build.iResourceOptional "excerpt" (Build.iMatch "article-" <> Build.iBind "name")
            )
          <*> Build.iAll (Build.iResource "note" Build.iAny)
      )
      (Build.oResource "feed" (Build.oMatch "index"))
      indexFeed
    <> Build.rule
      "sitemap"
      ( (,,)
          <$> Build.iResource "config" (Build.iMatch "sitemap-url")
          <*> Build.iResource "template" (Build.iMatch "sitemap.xml.temple")
          <*> Build.iResourceAll "html" Build.iAny
      )
      (Build.oResource "xml" (Build.oMatch "sitemap"))
      sitemap
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
    <> Build.rule
      "feed-route"
      (Build.iResource "feed" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "feed-" *< Build.oBind "name"))
      resourceRoute
    <> Build.rule
      "xml-route"
      (Build.iResource "xml" (Build.iBind "name"))
      (Build.oResource "route" (Build.oMatch "xml-" *< Build.oBind "name"))
      resourceRoute

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

    currentTemplate = Temple.TemplateRef . renderName . resourceName $ Build.resourceInputId iTemplate

  (deps, bindings, _template'') <- loadTemplate readTemplateRef renderTemplateRef iTemplate

  {-
  If a template statically depends on some resources, then we check that
  those resources exist when the template is uploaded. These resources are
  also registered as dependencies of the template.
  -}
  mResources <-
    for (find ((fromString "resource" ==) . Temple.bindingName) bindings) $ \binding ->
      fmap snd . runWriterT $
        runProviderT
          renderTemplateRef
          (lift . getTemplateRef)
          ("(" ++ renderResourceId (Build.resourceInputId iTemplate) ++ ")")
          (provideBinding (lift . readTemplateRef) currentTemplate binding trackedResourceField)

  let
    templateDependencies =
      Set.map
        (\(Temple.TemplateRef r) -> ResourceId (unsafeName "template") (unsafeName r))
        (Map.keysSet deps)
        <> fold mResources
  Build.setDependencies (Build.resourceInputId iTemplate) templateDependencies

renderMetadataPath :: [Metadata.Part] -> String
renderMetadataPath [] = ""
renderMetadataPath (Metadata.PIndex ix : rest) = "[" ++ show ix ++ "]" ++ renderMetadataPath rest

renderMetadataDecodeError :: Metadata.DecodeError -> String
renderMetadataDecodeError (Metadata.DecodeError path err) =
  renderMetadataPath path ++ ": " ++ err

requireMetadata ::
  Monad m =>
  Build.ResourceInput m a ->
  -- | Property name
  Text ->
  MetadataValueDecoder b ->
  Build.ActionT m b
requireMetadata input name decoder =
  case Map.lookup name (Build.resourceInputMetadata input) of
    Just value ->
      case runMetadataValueDecoder decoder [] value of
        Left err ->
          throwError . DiagnosticSimple $
            "("
              ++ renderResourceId (Build.resourceInputId input)
              ++ ":metadata:"
              ++ Text.unpack name
              ++ "): "
              ++ renderMetadataDecodeError err
        Right b ->
          pure b
    Nothing ->
      throwError . DiagnosticSimple $
        "("
          ++ renderResourceId (Build.resourceInputId input)
          ++ ":metadata): missing property '"
          ++ Text.unpack name
          ++ "'"

optionalMetadata ::
  Monad m =>
  Build.ResourceInput m a ->
  -- | Property name
  Text ->
  MetadataValueDecoder b ->
  Build.ActionT m (Maybe b)
optionalMetadata input name decoder =
  case Map.lookup name (Build.resourceInputMetadata input) of
    Just value ->
      case runMetadataValueDecoder decoder [] value of
        Left err ->
          throwError . DiagnosticSimple $
            "("
              ++ renderResourceId (Build.resourceInputId input)
              ++ ":metadata:"
              ++ Text.unpack name
              ++ "): "
              ++ renderMetadataDecodeError err
        Right b ->
          pure $ Just b
    Nothing ->
      pure Nothing

requirePublished ::
  Monad m =>
  Build.ResourceInput m a ->
  Build.ActionT m UTCTime
requirePublished input = do
  published <- requireMetadata input (fromString "published") Metadata.text
  case parseDateTime published <|> parseDate published of
    Just x -> pure x
    Nothing ->
      throwError . DiagnosticSimple $
        "("
          ++ renderResourceId (Build.resourceInputId input)
          ++ ":metadata:published): invalid date: "
          ++ Text.unpack published
  where
    parseDateTime :: Text -> Maybe UTCTime
    parseDateTime = iso8601ParseM . Text.unpack

    parseDate :: Text -> Maybe UTCTime
    parseDate x = do
      day <- iso8601ParseM $ Text.unpack x
      pure $ UTCTime day 0

articleAdjacency ::
  MonadIO m =>
  (Build.ResourceInputs m ByteString, Build.ResourceInputs m ByteString) ->
  Build.ResourceOutput m String ->
  Build.ActionT m ()
articleAdjacency (iArticles, iNotes) oAdjacency = do
  articlesWithPublished <- getResourcesWithPublished iArticles
  notesWithPublished <- getResourcesWithPublished iNotes

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
      Monad m =>
      Build.ResourceInputs m a ->
      Build.ActionT m [(UTCTime, ResourceId)]
    getResourcesWithPublished inputs =
      for (Build.resourceInputs inputs) $ \input ->
        (,Build.resourceInputId input) <$> requirePublished input

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

textToName :: Text -> Name
textToName = unsafeName . Text.unpack

nameToText :: Name -> Text
nameToText = fromString . renderName

formatContent :: Maybe MetadataValue -> ByteString -> ByteString
formatContent mFormat =
  case mFormat of
    Just (VString s) | s == fromString "text" -> id
    Just (VString s) | s == fromString "line" -> \x -> ByteString.Char8.dropWhileEnd (`elem` "\r\n") x
    _ -> id

getFormattedContent ::
  Monad m =>
  Build.ResourceInput m ByteString ->
  Build.ActionT m ByteString
getFormattedContent res = do
  let content = Build.resourceInputContent res
  mFormat <- Build.resourceInputProperty res (unsafeName "content-format")
  pure $ formatContent mFormat content

data AdjacencyValue
  = AdjacencyValue
  { adjacencyValueTitle :: !Text
  , adjacencyValueUrl :: !Text
  }

adjacencyValue :: Monad m => AdjacencyValue -> Value m
adjacencyValue adjacency =
  recordValue
    [ (fromString "title", textValue $ adjacencyValueTitle adjacency)
    , (fromString "url", textValue $ adjacencyValueUrl adjacency)
    ]

articleFields ::
  Monad m =>
  -- | Previous
  Maybe AdjacencyValue ->
  -- | Next
  Maybe AdjacencyValue ->
  -- | Rendered article content
  Text ->
  Fields m
articleFields mPrev mNext content =
  fields
    [ (fromString "previous", optionalValue $ adjacencyValue <$> mPrev)
    , (fromString "next", optionalValue $ adjacencyValue <$> mNext)
    , (fromString "content", textValue content)
    ]

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
      Left err ->
        throwError . DiagnosticSimple $
          "resource "
            ++ renderResourceId (Build.resourceInputId input)
            ++ " is not valid UTF-8: "
            ++ show err
      Right x -> pure x

  document <- pandoc $ Pandoc.readMarkdown markdownReaderOptions content
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
                (\acc name -> Temple.CField acc name)
                (Temple.CVar $ fromString "resource")
                (resTyName : resName : propertyPath)

            ty =
              foldr
                ( \name rest ->
                    Temple.TRecord $ Temple.TRecordField name rest Temple.TRowEnd
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

      let name = fromString "resource"
      providedCore <-
        runProviderT
          renderTemplateRef
          getTemplateRef
          ("(" ++ renderResourceId (Build.resourceInputId input) ++ ")")
          $ provideTypeScheme readTemplateRef currentTemplateRef name (Temple.Forall [] ty) trackedResourceValue

      let env = Temple.defaultEvalEnv mempty
      let env' = env{Temple.eeScope = Map.singleton name (Temple.evalCore env providedCore)}
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
      let templateLocation = "(" ++ renderResourceId (Build.resourceInputId input) ++ ", inline HTML)"
      (deps, bindings, template') <-
        lift $
          inferBindings
            readTemplateRef
            renderTemplateRef
            getTemplateRef
            (templateLocation, content')
            inputRef
            template

      -- TODO: Template references in inline HTML are currently unsupported
      unless (null deps) . error $
        "unexpected template dependencies in " ++ show template

      bindings' <- for bindings $ \binding ->
        (,) (Temple.bindingName binding)
          <$> runProviderT
            renderTemplateRef
            getTemplateRef
            templateLocation
            (provideBinding readTemplateRef inputRef binding trackedResourceField)

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

loadMetadata ::
  MonadError DiagnosticReports m =>
  Store.ResourceType m ->
  -- | Resource name
  Name ->
  m (Map Text MetadataValue)
loadMetadata resTy resName = do
  mContent <- Store.readProperty resTy resName (unsafeName "metadata")
  case mContent of
    Nothing ->
      pure mempty
    Just content ->
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

getAdjacency ::
  MonadIO m =>
  Build.ResourceInput m ByteString ->
  Build.ActionT m (Maybe AdjacencyValue, Maybe AdjacencyValue)
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

      resTy <- Store.getResourceType store (Just xactId) resTyName
      metadata <- loadMetadata resTy resName
      let
        requireString name =
          case Map.lookup (fromString name) metadata of
            Just (VString s) ->
              pure s
            Just _ ->
              throwError . DiagnosticSimple $
                "(" ++ renderResourceId resId ++ ":metadata:" ++ name ++ "): not a string"
            Nothing ->
              throwError . DiagnosticSimple $
                "(" ++ renderResourceId resId ++ ":metadata): missing '" ++ name ++ "'"

      AdjacencyValue <$> requireString "title" <*> requireString "url"

  prev' <- traverse getAdjacencyFields prev
  next' <- traverse getAdjacencyFields next

  pure (prev', next')

metadataValue :: Monad m => MetadataValue -> Value m
metadataValue VTrue = boolValue True
metadataValue VFalse = boolValue False
metadataValue (VString s) = textValue s
metadataValue (VList items) = List $ metadataValue <$> items
metadataValue (VConstructor name args) = Constructor name $ metadataValue <$> args
metadataValue (VRecord record) = recordValue $ (fmap . fmap) metadataValue record

metadataFields :: Monad m => Map Text MetadataValue -> Fields m
metadataFields metadata = Fields $ \_path name -> pure $ Map.lookup name metadata'
  where
    metadata' = metadataValue <$> metadata

makePropertyFields ::
  Monad m =>
  -- | Get the @content@ property
  m ByteString ->
  -- | Get the resource's metadata
  m (Map Text MetadataValue) ->
  -- | Look up a property's value
  (Name -> m (Maybe MetadataValue)) ->
  Fields m
makePropertyFields getContent getMetadata lookupProperty =
  Fields $ \_path name ->
    lift $
      case Text.unpack name of
        "content" ->
          Just . bytestringValue <$> getContent
        "metadata" ->
          Just . Record . metadataFields <$> getMetadata
        _ ->
          fmap metadataValue <$> lookupProperty (textToName name)

propertyFields :: Monad m => Build.ResourceInput m ByteString -> Fields (Build.ActionT m)
propertyFields res =
  makePropertyFields
    (getFormattedContent res)
    (pure $ Build.resourceInputMetadata res)
    (Build.resourceInputProperty res)

resourceField :: Monad m => Fields (Build.ActionT m)
resourceField = fields [(fromString "resource", resourceValue id (\_ -> pure ()))]

trackedResourceField :: Monad m => Fields (WriterT (Set ResourceId) (Build.ActionT m))
trackedResourceField = fields [(fromString "resource", trackedResourceValue)]

trackedResourceValue :: Monad m => Value (WriterT (Set ResourceId) (Build.ActionT m))
trackedResourceValue = resourceValue lift (tell . Set.singleton)

resourceValue ::
  forall m n.
  (Monad m, Monad n) =>
  -- | Run a build action
  (forall a. Build.ActionT m a -> n a) ->
  -- | Record that a resource was referenced
  (ResourceId -> n ()) ->
  Value n
resourceValue liftAction track =
  Record . Fields $ \path resTyName -> do
    mResTy <- action $ do
      store <- Build.askStore
      xactId <- Build.askTransactionId
      Store.lookupResourceType store (Just xactId) (textToName resTyName)
    resTy <- maybe (throwError $ ResourceTypeNotFound path resTyName) pure mResTy

    pure . Just $ resourceTypeValue resTy
  where
    action :: Build.ActionT m a -> ProviderT n a
    action = lift . liftAction

    resourceTypeValue resTy =
      Record . Fields $ \path resName -> do
        exists <- action $ Store.doesResourceExist resTy (textToName resName)
        unless exists . throwError $ ResourceNotFound path resName
        lift . track $ ResourceId (Store.resourceTypeName resTy) (textToName resName)

        pure . Just $ resourcePropertiesValue resTy resName

    resourcePropertiesValue resTy resName =
      Record $
        makePropertyFields
          ( liftAction $ do
              mContent <- Store.readResource resTy (textToName resName)
              case mContent of
                Nothing ->
                  pure mempty
                Just content -> do
                  mFormat <- Store.lookupProperty resTy (textToName resName) (unsafeName "content-format")
                  pure $ formatContent mFormat content
          )
          (liftAction $ loadMetadata resTy (textToName resName))
          (liftAction . Store.lookupProperty resTy (textToName resName))

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

  let metadata = Build.resourceInputMetadata iArticle

  output <-
    renderTemplate iTemplate $
      resourceField
        <> fields
          [
            ( fromString "self"
            , Record $
                articleFields prev next html
                  <> propertyFields iArticle
            )
          ]

  Build.writeResource oHtml () output

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

  let metadata = Build.resourceInputMetadata iNote

  output <-
    renderTemplate iTemplate $
      resourceField
        <> fields
          [
            ( fromString "self"
            , Record $
                articleFields prev next html
                  <> propertyFields iNote
            )
          ]

  Build.writeResource oHtml () output

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

  let metadata = Build.resourceInputMetadata iPage

  output <-
    renderTemplate iTemplate $
      resourceField
        <> fields
          [
            ( fromString "self"
            , Record $
                fields [(fromString "content", textValue html)]
                  <> propertyFields iPage
            )
          ]

  Build.writeResource oHtml () output

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
      -- | @references@ metadata
      MetadataValue

sortPosts ::
  MonadIO m =>
  [(Build.ResourceInput m ByteString, Maybe (Build.ResourceInput m ByteString))] ->
  [Build.ResourceInput m ByteString] ->
  Build.ActionT m [IndexItem m]
sortPosts iArticlesWithExcerpts iNotes = do
  articlesWithExcerptsWithPublished <-
    for
      iArticlesWithExcerpts
      ( \(iArticle, miExcerpt) -> do
          published <- requirePublished iArticle
          pure (published, IndexArticle iArticle miExcerpt)
      )

  notesWithPublished <-
    for
      iNotes
      ( \iNote -> do
          published <- requirePublished iNote
          references <- requireMetadata iNote (fromString "references")
          (_deps, document) <- loadMarkdown iNote
          html <- renderHtml document
          pure (published, IndexNote iNote html references)
      )

  pure
    . fmap snd
    . sortOn (Down . fst)
    $ articlesWithExcerptsWithPublished ++ notesWithPublished

postsList :: Monad m => [IndexItem m] -> Value (Build.ActionT m)
postsList sortedPosts =
  List $
    sortedPosts <&> \case
      IndexArticle iArticle miExcerpt ->
        post (Build.resourceInputMetadata iArticle) . Constructor (fromString "Article") $
          [ recordValue
              [ (fromString "excerpt", bytestringValue $ Build.resourceInputContent iExcerpt)
              | Just iExcerpt <- [miExcerpt]
              ]
          ]
      IndexNote iNote html references ->
        post (Build.resourceInputMetadata iNote) . Constructor (fromString "Note") $
          [ recordValue
              [ (fromString "content", textValue html)
              , (fromString "references", metadataValue references)
              ]
          ]
  where
    -- TODO: the `type` property should come from metadata.
    --
    -- Currently blocked on having a good syntax for sum types in metadata.
    post metadata postType =
      recordValue
        [
          ( fromString "metadata"
          , Record $ fields [(fromString "type", postType)] <> metadataFields metadata
          )
        ]

indexHtml ::
  MonadIO m =>
  ( Build.ResourceInput m ByteString
  , [(Build.ResourceInput m ByteString, Maybe (Build.ResourceInput m ByteString))]
  , [Build.ResourceInput m ByteString]
  ) ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
indexHtml (iTemplate, iArticlesWithExcerpts, iNotes) oHtml = do
  sortedPosts <- sortPosts iArticlesWithExcerpts iNotes

  output <-
    renderTemplate iTemplate $
      resourceField
        <> fields
          [
            ( fromString "self"
            , recordValue
                [
                  ( fromString "metadata"
                  , recordValue
                      [ (fromString "url", stringValue "/")
                      , (fromString "title", stringValue "blog.ielliott.io")
                      , (fromString "description", stringValue "Isaac Elliott's personal blog.")
                      , (fromString "math", boolValue False)
                      , (fromString "chinese", boolValue False)
                      , (fromString "asciinema", boolValue False)
                      ]
                  )
                ]
            )
          ,
            ( fromString "tag"
            , optionalValue Nothing
            )
          ,
            ( fromString "posts"
            , postsList sortedPosts
            )
          ]

  Build.writeResource oHtml () output

  Build.setResourceProperty oHtml () (unsafeName "url") $ VString (fromString "/")

requireStringProperty ::
  Monad m =>
  Build.ResourceInput m a ->
  -- | Property name
  Name ->
  Build.ActionT m Text
requireStringProperty res name = do
  mValue <- Build.resourceInputProperty res name
  case mValue of
    Nothing ->
      throwError . DiagnosticSimple $
        "("
          ++ renderResourceId (Build.resourceInputId res)
          ++ "): missing property '"
          ++ renderName name
          ++ "'"
    Just (VString s) -> pure s
    Just _value ->
      throwError . DiagnosticSimple $
        "(" ++ renderResourceId (Build.resourceInputId res) ++ ":" ++ renderName name ++ "): not a string"

indexFeed ::
  MonadIO m =>
  ( Build.ResourceInput m ByteString
  , Build.ResourceInput m ByteString
  , [(Build.ResourceInput m ByteString, Maybe (Build.ResourceInput m ByteString))]
  , [Build.ResourceInput m ByteString]
  ) ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
indexFeed (iFeedConfig, iTemplate, iArticlesWithExcerpts, iNotes) oFeed = do
  url <- requireStringProperty iFeedConfig (unsafeName "url")
  title <- requireStringProperty iFeedConfig (unsafeName "title")
  description <- requireStringProperty iFeedConfig (unsafeName "description")
  authorName <- requireStringProperty iFeedConfig (unsafeName "authorName")
  authorEmail <- requireStringProperty iFeedConfig (unsafeName "authorEmail")

  sortedPosts <- sortPosts iArticlesWithExcerpts iNotes

  now <- liftIO getCurrentTime

  output <-
    renderTemplate iTemplate $
      resourceField
        <> fields
          [ (fromString "url", textValue url)
          , (fromString "title", textValue title)
          , (fromString "description", textValue description)
          , (fromString "authorName", textValue authorName)
          , (fromString "authorEmail", textValue authorEmail)
          , (fromString "updated", stringValue $ renderUTCTime now)
          , (fromString "posts", postsList sortedPosts)
          ]

  Build.writeResource oFeed () output

  Build.setResourceProperty oFeed () (unsafeName "url") $ VString url

sitemap ::
  MonadIO m =>
  ( Build.ResourceInput m ByteString
  , Build.ResourceInput m ByteString
  , Build.ResourceInputs m ByteString
  ) ->
  Build.ResourceOutput m () ->
  Build.ActionT m ()
sitemap (iSitemapUrl, iTemplate, iHtmls) oXml = do
  url <- getFormattedContent iSitemapUrl

  output <-
    renderTemplate iTemplate $
      resourceField
        <> fields
          [
            ( fromString "pages"
            , List $
                Build.resourceInputs iHtmls <&> \iHtml ->
                  -- TODO: add `<lastmod>` to sitemap URLs by passing the `updated`
                  -- property. Requires that either every page has an `updated`
                  -- property, or there's a way to get a reasonable default (getModificationTime?)
                  Record $ propertyFields iHtml
            )
          ]

  Build.writeResource oXml () output

  Build.setResourceProperty oXml () (unsafeName "url") $ VString (Text.Encoding.decodeUtf8 url)

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
    {-
    At first glance it seemed wasteful to parse the `url` property only to
    immediately render it again. One advantage, though, is that it validates
    the `url` property here instead of letting an invalid URL through into
    the `route` resource.
    -}
    path <-
      case url of
        VString s -> do
          let input = Text.Encoding.encodeUtf8 s
          case Routes.parsePath input of
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
