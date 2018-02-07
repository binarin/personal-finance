module UIStyle (writeCss) where

import Clay
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy.IO as T

writeCss :: FilePath -> IO ()
writeCss path = T.writeFile path css

css :: Text
css = renderWith pretty [] stylesheet

stylesheet :: Css
stylesheet = do
    reconcillation
    date
    expense
    income
    transfer

date :: Css
date = do
    ".date" ? do
        pure ()
    ".date__prev" ? do
       display inline
    ".date__date" ? do
       display inline
    ".date__next" ? do
       display inline

transactionDetails :: Css
transactionDetails = do
  paddingLeft (em 0.5)
  margin (em 1) (em 1) (em 1) (em 1)
  display grid
  "grid-template-columns" -: "1fr 1fr"
  borderBottom solid (px 3) (lighten 0.4 gray)
  borderLeft solid (em 2) (lighten 0.4 gray)

expense :: Css
expense = do
  ".expense" ? do
    transactionDetails
  ".expense__category" ? do
    display inline
  ".expense__amount" ? do
    marginLeft auto
    display inline
    color red
    fontWeight bold

income :: Css
income = do
  ".income" ? do
    transactionDetails
  ".income__category" ? do
    display inline
  ".income__amount" ? do
    marginLeft auto
    display inline
    color green
    fontWeight bold

transfer :: Css
transfer = do
  ".transfer" ? do
    transactionDetails
  ".transfer__category" ? do
    display inline
  ".transfer__amount" ? do
    marginLeft auto
    display inline
    color grey
    fontWeight bold



reconcillation :: Css
reconcillation = do
    ".reconcillation" ? do
        display grid
        "grid-template-columns" -: "2fr 1fr 2fr"
    ".reconcillation__toshl" ? do
        "grid-row" -: "1 / span 2"
        "grid-column" -: "1"
    ".reconcillation__bank" ? do
        "grid-row" -: "1 / span 2"
        "grid-column" -: "3"
    ".reconcillation__date" ? do
        "grid-row" -: "1"
        "grid-column" -: "2"
    ".reconcillation__editor" ? do
        "grid-row" -: "2"
        "grid-column" -: "2"
