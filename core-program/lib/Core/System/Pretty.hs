{-# OPTIONS_HADDOCK not-home #-}

--
-- | Re-exports of combinators for use when building 'Render' instances.
--
module Core.System.Pretty
    ( {-* Pretty Printing -}
      {-** from Data.Text.Prettyprint.Doc -}
      {-| Re-exported from "Data.Text.Prettyprint.Doc" in __prettyprinter__
      and "Data.Text.Prettyprint.Doc.Render.Terminal" in
      __prettyprinter-ansi-terminal__: -}
      Doc
    , Pretty(pretty)
    , dquote
    , squote
    , comma
    , punctuate
    , enclose
    , lbracket
    , rbracket
    , (<+>)
    , lbrace
    , rbrace
    , lparen
    , rparen
    , emptyDoc
    , sep
    , hsep
    , vsep
    , fillSep
    , flatAlt
    , hcat
    , vcat
    , annotate
    , unAnnotate
    , line
    , line'
    , softline
    , softline'
    , hardline
    , group
    , hang
    , indent
    , nest
    , concatWith
    , color
    , colorDull
    , Color(..)
    , AnsiStyle
    , bold
    ) where

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal
