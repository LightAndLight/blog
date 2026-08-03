{-# LANGUAGE FlexibleContexts #-}

module Blog.Diagnostic
  ( DiagnosticReports (..)
  , Reports (..)
  , renderDiagnosticReports
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Lazy (LazyByteString)
import Data.String (fromString)
import qualified Text.Diagnostic as Diagnostic

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
