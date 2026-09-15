{-# LANGUAGE FlexibleContexts #-}

module Blog.Pandoc (markdownReaderOptions, htmlWriterOptions, pandoc) where

import Blog.Diagnostic (DiagnosticReports (..))
import Control.Monad.Error.Class (MonadError, throwError)
import Data.String (fromString)
import qualified Data.Text as Text
import Text.Pandoc
import Text.Pandoc.Highlighting (pygments)

markdownReaderOptions :: ReaderOptions
markdownReaderOptions =
  def
    { readerExtensions =
        -- Syntax for adding attributes to Markdown elements.
        -- `tableOfContents` uses this to omit a heading's children from the contents listing.
        enableExtension Ext_header_attributes
          . enableExtension Ext_link_attributes
          -- Uses `Div` blocks for `<div>` tags so that I can post-process them.
          -- See `tableOfContents` for an example.
          . enableExtension Ext_native_divs
          . enableExtension Ext_backtick_code_blocks
          . enableExtension Ext_markdown_in_html_blocks
          . enableExtension Ext_auto_identifiers
          . enableExtension Ext_gfm_auto_identifiers
          $ getDefaultExtensions (fromString "commonmark_x")
    }

htmlWriterOptions :: WriterOptions
htmlWriterOptions =
  def
    { writerHighlightMethod = Skylighting pygments
    , writerWrapText = WrapPreserve
    , writerExtensions = enableExtension Ext_tex_math_dollars (writerExtensions def)
    , writerHTMLMathMethod = MathML
    }

pandoc :: MonadError DiagnosticReports m => PandocPure a -> m a
pandoc = either (throwError . DiagnosticSimple . Text.unpack . renderError) pure . runPure
