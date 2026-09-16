module Blog.Time (renderUTCTime) where

import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)

renderUTCTime :: UTCTime -> String
renderUTCTime = formatTime defaultTimeLocale "%FT%H:%M:%SZ"
