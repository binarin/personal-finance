module UI.BankTransactionsCSS (css) where

import Clay

css :: Css
css = do
  ".bank-transaction" ? do
    paddingLeft (em 0.5)
    margin (em 1) (em 1) (em 1) (em 1)
    display grid
    height (em 6)
    "grid-template-columns" -: "1fr 1fr"
    borderBottom solid (px 3) (lighten 0.4 gray)
    borderLeft solid (em 2) (lighten 0.4 gray)

  ".bank-transaction__description" ? do
    "grid-column" -: "1 / span 2"

  ".bank-transaction__date" ? do
    marginLeft auto
