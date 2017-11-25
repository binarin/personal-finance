module UIStyle (writeCss) where

import Clay
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T

writeCss :: FilePath -> IO ()
writeCss path = T.writeFile path css

css :: Text
css = renderWith pretty [] stylesheet

stylesheet :: Css
stylesheet =
    body ? do
        background black
