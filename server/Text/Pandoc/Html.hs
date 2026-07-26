{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Pandoc.Html
  ( renderPandoc
  , RenderT
  , runRenderT
  , NotesState (..)
  , emptyNotesState
  , renderBlocks
  , renderBlock
  , renderInline
  )
where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Strict (StateT, evalStateT, state)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Text.Lazy (LazyText)
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as Builder
import Text.Pandoc.Definition

renderPandoc :: Monad m => Pandoc -> m LazyText
renderPandoc (Pandoc _ blocks) =
  fmap Builder.toLazyText . runRenderT emptyNotesState $ do
    body <- renderBlocks blocks
    footnotes <- renderFootnotes
    pure $ body <> footnotes

newtype RenderT m a = RenderT (StateT NotesState m a)
  deriving (Functor, Applicative, Monad, MonadIO)

runRenderT :: Monad m => NotesState -> RenderT m a -> m a
runRenderT s (RenderT ma) = evalStateT ma s

data NotesState = NotesState
  { noteCount :: Int
  , pendingNotes :: [(Int, [Block])]
  }

emptyNotesState :: NotesState
emptyNotesState = NotesState 0 []

renderBlocks :: Monad m => [Block] -> RenderT m Builder
renderBlocks = fmap mconcat . traverse renderBlock

renderBlock :: Monad m => Block -> RenderT m Builder
renderBlock block =
  case block of
    Plain inlines ->
      (<> "\n") <$> renderInlines inlines
    Para inlines -> do
      content <- renderInlines inlines
      pure $ "<p>" <> content <> "</p>\n"
    LineBlock lns -> do
      lns' <- traverse renderInlines lns
      pure $
        "<div class=\"line-block\">"
          <> mconcat (intersperse "<br />\n" lns')
          <> "</div>\n"
    CodeBlock attr code ->
      pure $
        "<pre"
          <> renderAttr attr
          <> "><code>"
          <> escapeText code
          <> "</code></pre>\n"
    RawBlock format content
      | isHtmlFormat format -> pure $ Builder.fromText content <> "\n"
      | otherwise -> pure mempty
    BlockQuote blocks -> do
      content <- renderBlocks blocks
      pure $ "<blockquote>\n" <> content <> "</blockquote>\n"
    OrderedList (start, numberStyle, _) items -> do
      items' <- traverse renderListItem items
      pure $
        "<ol"
          <> (if start == 1 then mempty else " start=\"" <> Builder.fromString (show start) <> "\"")
          <> orderedListType numberStyle
          <> ">\n"
          <> mconcat items'
          <> "</ol>\n"
    BulletList items -> do
      items' <- traverse renderListItem items
      pure $ "<ul>\n" <> mconcat items' <> "</ul>\n"
    DefinitionList items -> do
      items' <- traverse renderDefinition items
      pure $ "<dl>\n" <> mconcat items' <> "</dl>\n"
    Header level attr inlines -> do
      content <- renderInlines inlines
      let tag = "h" <> Builder.fromString (show $ max 1 (min 6 level))
      pure $ "<" <> tag <> renderAttr attr <> ">" <> content <> "</" <> tag <> ">\n"
    HorizontalRule ->
      pure "<hr />\n"
    Table attr caption colSpecs tableHead tableBodies tableFoot ->
      renderTable attr caption colSpecs tableHead tableBodies tableFoot
    Figure attr (Caption _ captionBlocks) blocks -> do
      content <- renderBlocks blocks
      caption <-
        if null captionBlocks
          then pure mempty
          else do
            caption <- renderBlocks captionBlocks
            pure $ "<figcaption>\n" <> caption <> "</figcaption>\n"
      pure $ "<figure" <> renderAttr attr <> ">\n" <> content <> caption <> "</figure>\n"
    Div attr blocks -> do
      content <- renderBlocks blocks
      pure $ "<div" <> renderAttr attr <> ">\n" <> content <> "</div>\n"

renderListItem :: Monad m => [Block] -> RenderT m Builder
renderListItem item = do
  content <- renderBlocks item
  pure $ "<li>" <> content <> "</li>\n"

renderDefinition :: Monad m => ([Inline], [[Block]]) -> RenderT m Builder
renderDefinition (term, definitions) = do
  term' <- renderInlines term
  definitions' <- traverse renderBlocks definitions
  pure $
    "<dt>"
      <> term'
      <> "</dt>\n"
      <> mconcat (map (\d -> "<dd>" <> d <> "</dd>\n") definitions')

orderedListType :: ListNumberStyle -> Builder
orderedListType numberStyle =
  case numberStyle of
    Decimal -> " type=\"1\""
    LowerAlpha -> " type=\"a\""
    UpperAlpha -> " type=\"A\""
    LowerRoman -> " type=\"i\""
    UpperRoman -> " type=\"I\""
    DefaultStyle -> mempty
    Example -> mempty

renderTable ::
  Monad m =>
  Attr ->
  Caption ->
  [ColSpec] ->
  TableHead ->
  [TableBody] ->
  TableFoot ->
  RenderT m Builder
renderTable attr (Caption _ captionBlocks) colSpecs tableHead tableBodies tableFoot = do
  caption <-
    if null captionBlocks
      then pure mempty
      else do
        caption <- renderBlocks captionBlocks
        pure $ "<caption>\n" <> caption <> "</caption>\n"
  head' <- renderHead tableHead
  bodies <- mconcat <$> traverse renderBody tableBodies
  foot <- renderFoot tableFoot
  pure $
    "<table"
      <> renderAttr attr
      <> ">\n"
      <> caption
      <> colgroup
      <> head'
      <> bodies
      <> foot
      <> "</table>\n"
  where
    colgroup
      | all ((ColWidthDefault ==) . snd) colSpecs = mempty
      | otherwise =
          "<colgroup>\n" <> mconcat (map renderCol colSpecs) <> "</colgroup>\n"

    renderCol (_, ColWidthDefault) = "<col />\n"
    renderCol (_, ColWidth w) =
      "<col style=\"width: " <> Builder.fromString (show $ w * 100) <> "%\" />\n"

    renderHead (TableHead headAttr rows)
      | null rows = pure mempty
      | otherwise = do
          rows' <- renderRows "th" rows
          pure $ "<thead" <> renderAttr headAttr <> ">\n" <> rows' <> "</thead>\n"

    renderBody (TableBody bodyAttr _ headerRows rows) = do
      headerRows' <- renderRows "th" headerRows
      rows' <- renderRows "td" rows
      pure $ "<tbody" <> renderAttr bodyAttr <> ">\n" <> headerRows' <> rows' <> "</tbody>\n"

    renderFoot (TableFoot footAttr rows)
      | null rows = pure mempty
      | otherwise = do
          rows' <- renderRows "td" rows
          pure $ "<tfoot" <> renderAttr footAttr <> ">\n" <> rows' <> "</tfoot>\n"

    renderRows tag = fmap mconcat . traverse (renderRow tag)

    renderRow tag (Row rowAttr cells) = do
      cells' <- mconcat <$> traverse (renderCell tag) cells
      pure $ "<tr" <> renderAttr rowAttr <> ">\n" <> cells' <> "</tr>\n"

    renderCell tag (Cell cellAttr align (RowSpan rowSpan) (ColSpan colSpan) blocks) = do
      content <- renderBlocks blocks
      pure $
        "<"
          <> tag
          <> renderAttr cellAttr
          <> (if rowSpan == 1 then mempty else " rowspan=\"" <> Builder.fromString (show rowSpan) <> "\"")
          <> (if colSpan == 1 then mempty else " colspan=\"" <> Builder.fromString (show colSpan) <> "\"")
          <> alignStyle align
          <> ">"
          <> content
          <> "</"
          <> tag
          <> ">\n"

    alignStyle align =
      case align of
        AlignLeft -> " style=\"text-align: left;\""
        AlignRight -> " style=\"text-align: right;\""
        AlignCenter -> " style=\"text-align: center;\""
        AlignDefault -> mempty

renderInlines :: Monad m => [Inline] -> RenderT m Builder
renderInlines = fmap mconcat . traverse renderInline

renderInline :: Monad m => Inline -> RenderT m Builder
renderInline inline =
  case inline of
    Str t ->
      pure $ escapeText t
    Emph inlines ->
      wrap "<em>" "</em>" inlines
    Underline inlines ->
      wrap "<u>" "</u>" inlines
    Strong inlines ->
      wrap "<strong>" "</strong>" inlines
    Strikeout inlines ->
      wrap "<del>" "</del>" inlines
    Superscript inlines ->
      wrap "<sup>" "</sup>" inlines
    Subscript inlines ->
      wrap "<sub>" "</sub>" inlines
    SmallCaps inlines ->
      wrap "<span class=\"smallcaps\">" "</span>" inlines
    Quoted SingleQuote inlines ->
      wrap "\8216" "\8217" inlines
    Quoted DoubleQuote inlines ->
      wrap "\8220" "\8221" inlines
    Cite citations inlines -> do
      let ids = Text.unwords $ map citationId citations
      wrap
        ("<span class=\"citation\" data-cites=\"" <> escapeText ids <> "\">")
        "</span>"
        inlines
    Code attr code ->
      pure $ "<code" <> renderAttr attr <> ">" <> escapeText code <> "</code>"
    Space ->
      pure " "
    SoftBreak ->
      pure "\n"
    LineBreak ->
      pure "<br />\n"
    Math InlineMath code ->
      pure $ "<span class=\"math inline\">\\(" <> escapeText code <> "\\)</span>"
    Math DisplayMath code ->
      pure $ "<span class=\"math display\">\\[" <> escapeText code <> "\\]</span>"
    RawInline format content
      | isHtmlFormat format -> pure $ Builder.fromText content
      | otherwise -> pure mempty
    Link attr inlines (url, title) ->
      wrap
        ( "<a href=\""
            <> escapeText url
            <> "\""
            <> (if Text.null title then mempty else " title=\"" <> escapeText title <> "\"")
            <> renderAttr attr
            <> ">"
        )
        "</a>"
        inlines
    Image attr alt (url, title) ->
      pure $
        "<img src=\""
          <> escapeText url
          <> "\""
          <> (if Text.null title then mempty else " title=\"" <> escapeText title <> "\"")
          <> " alt=\""
          <> escapeText (plaintext alt)
          <> "\""
          <> renderAttr attr
          <> " />"
    Note blocks -> do
      n <-
        RenderT . state $ \s ->
          let n = noteCount s + 1
          in (n, NotesState n ((n, blocks) : pendingNotes s))
      pure $
        "<a href=\"#fn"
          <> Builder.fromString (show n)
          <> "\" class=\"footnote-ref\" id=\"fnref"
          <> Builder.fromString (show n)
          <> "\" role=\"doc-noteref\"><sup>"
          <> Builder.fromString (show n)
          <> "</sup></a>"
    Span attr inlines ->
      wrap ("<span" <> renderAttr attr <> ">") "</span>" inlines
  where
    wrap before after inlines = do
      content <- renderInlines inlines
      pure $ before <> content <> after

renderFootnotes :: Monad m => RenderT m Builder
renderFootnotes = do
  items <- go
  pure $
    if null items
      then mempty
      else
        "<section id=\"footnotes\" class=\"footnotes\" role=\"doc-endnotes\">\n<hr />\n<ol>\n"
          <> mconcat items
          <> "</ol>\n</section>\n"
  where
    go = do
      pending <- RenderT . state $ \s -> (reverse (pendingNotes s), s{pendingNotes = []})
      if null pending
        then pure []
        else do
          items <- traverse renderFootnote pending
          (items ++) <$> go

    renderFootnote (n, blocks) = do
      content <- renderBlocks (addFootnoteBacklink n blocks)
      pure $ "<li id=\"fn" <> Builder.fromString (show n) <> "\">" <> content <> "</li>\n"

addFootnoteBacklink :: Int -> [Block] -> [Block]
addFootnoteBacklink n blocks =
  case reverse blocks of
    Para inlines : rest -> reverse rest ++ [Para (inlines ++ [Space, link])]
    Plain inlines : rest -> reverse rest ++ [Plain (inlines ++ [Space, link])]
    _ -> blocks ++ [Para [link]]
  where
    link =
      RawInline
        (Format "html")
        ( "<a href=\"#fnref"
            <> Text.pack (show n)
            <> "\" class=\"footnote-back\" role=\"doc-backlink\">\8617\65038</a>"
        )

renderAttr :: Attr -> Builder
renderAttr (identifier, classes, keyValues) =
  (if Text.null identifier then mempty else " id=\"" <> escapeText identifier <> "\"")
    <> ( if null classes
           then mempty
           else " class=\"" <> escapeText (Text.unwords classes) <> "\""
       )
    <> mconcat
      [" " <> escapeText key <> "=\"" <> escapeText value <> "\"" | (key, value) <- keyValues]

isHtmlFormat :: Format -> Bool
isHtmlFormat format = format == "html" || format == "html4" || format == "html5"

plaintext :: [Inline] -> Text
plaintext = Text.concat . map go
  where
    go inline =
      case inline of
        Str t -> t
        Emph inlines -> plaintext inlines
        Underline inlines -> plaintext inlines
        Strong inlines -> plaintext inlines
        Strikeout inlines -> plaintext inlines
        Superscript inlines -> plaintext inlines
        Subscript inlines -> plaintext inlines
        SmallCaps inlines -> plaintext inlines
        Quoted _ inlines -> plaintext inlines
        Cite _ inlines -> plaintext inlines
        Code _ t -> t
        Space -> " "
        SoftBreak -> " "
        LineBreak -> " "
        Math _ t -> t
        RawInline _ _ -> ""
        Link _ inlines _ -> plaintext inlines
        Image _ inlines _ -> plaintext inlines
        Note _ -> ""
        Span _ inlines -> plaintext inlines

escapeText :: Text -> Builder
escapeText = foldMap escapeChar . Text.unpack
  where
    escapeChar c =
      case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        _ -> Builder.singleton c
