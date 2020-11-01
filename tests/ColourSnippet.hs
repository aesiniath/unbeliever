{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

import Core.Data
import Core.Encoding
import Core.Program
import Core.System
import Core.Text
    ( brightBlue,
      brightCyan,
      brightGreen,
      brightGrey,
      brightMagenta,
      brightRed,
      brightWhite,
      brightYellow,
      dullBlue,
      dullCyan,
      dullGreen,
      dullGrey,
      dullMagenta,
      dullRed,
      dullWhite,
      dullYellow,
      pureBlack,
      pureBlue,
      pureCyan,
      pureGreen,
      pureGrey,
      pureMagenta,
      pureRed,
      pureWhite,
      pureYellow,
      Render(Token, highlight, colourize) )
import Data.Char (toLower)

data Colours
  = DullRed
  | BrightRed
  | PureRed
  | DullGreen
  | BrightGreen
  | PureGreen
  | DullBlue
  | BrightBlue
  | PureBlue
  | DullCyan
  | BrightCyan
  | PureCyan
  | DullMagenta
  | BrightMagenta
  | PureMagenta
  | DullYellow
  | BrightYellow
  | PureYellow
  | PureBlack
  | DullGrey
  | BrightGrey
  | PureGrey
  | DullWhite
  | BrightWhite
  | PureWhite
  deriving (Show)

instance Render Colours where
  type Token Colours = Colours
  colourize colour = case colour of
    DullRed -> dullRed
    BrightRed -> brightRed
    PureRed -> pureRed
    DullGreen -> dullGreen
    BrightGreen -> brightGreen
    PureGreen -> pureGreen
    DullBlue -> dullBlue
    BrightBlue -> brightBlue
    PureBlue -> pureBlue
    DullCyan -> dullCyan
    BrightCyan -> brightCyan
    PureCyan -> pureCyan
    DullMagenta -> dullMagenta
    BrightMagenta -> brightMagenta
    PureMagenta -> pureMagenta
    DullYellow -> dullYellow
    BrightYellow -> brightYellow
    PureYellow -> pureYellow
    PureBlack -> pureBlack
    DullGrey -> dullGrey
    BrightGrey -> brightGrey
    PureGrey -> pureGrey
    PureWhite -> pureWhite
    DullWhite -> dullWhite
    BrightWhite -> brightWhite

  -- this is contrived to exercise nesting formatting codes and returning out
  -- again.
  highlight colour =
    annotate
      colour
      ( pretty (originalName (show colour))
          <> (annotate BrightGrey "{wow}")
          <> pretty (originalName (show colour))
      )
      <> ", nice"
    where
      originalName (c : cs) = toLower c : cs

colours :: [Colours]
colours =
  [ DullRed,
    BrightRed,
    PureRed,
    DullYellow,
    BrightYellow,
    PureYellow,
    DullGreen,
    BrightGreen,
    PureGreen,
    DullCyan,
    BrightCyan,
    PureCyan,
    DullBlue,
    BrightBlue,
    PureBlue,
    DullMagenta,
    BrightMagenta,
    PureMagenta,
    DullWhite,
    BrightWhite,
    PureWhite,
    PureBlack,
    DullGrey,
    BrightGrey,
    PureGrey
  ]

main :: IO ()
main =
  execute $ do
    write "Here are the colours offered by Core.Text.Utilities:"
    mapM_ writeR colours
    write "We should now be back to normal."
