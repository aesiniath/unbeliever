{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Support for colour in the terminal.

![ANSI colours](AnsiColours.png)
-}
module Core.Text.Colour (
    AnsiColour,
    intoEscapes,
    boldColour,
    dullRed,
    brightRed,
    pureRed,
    dullGreen,
    brightGreen,
    pureGreen,
    dullBlue,
    brightBlue,
    pureBlue,
    dullCyan,
    brightCyan,
    pureCyan,
    dullMagenta,
    brightMagenta,
    pureMagenta,
    dullYellow,
    brightYellow,
    pureYellow,
    pureBlack,
    dullGrey,
    brightGrey,
    pureGrey,
    pureWhite,
    dullWhite,
    brightWhite,
    resetColour,
) where

import Core.Text.Rope
import Data.Colour.SRGB (sRGB, sRGB24read)
import System.Console.ANSI.Codes (setSGRCode)
import System.Console.ANSI.Types (ConsoleIntensity (..), ConsoleLayer (..), SGR (..))

{- |
An accumulation of ANSI escape codes used to add colour when pretty printing
to console.
-}
newtype AnsiColour = Escapes [SGR]

{-|
Convert an AnsiColour into the ANSI escape sequences which will make that
colour appear in the user's terminal.
-}
intoEscapes :: AnsiColour -> Rope
intoEscapes (Escapes codes) = intoRope (setSGRCode codes)

-- | Medium \"Scarlet Red\" (@#cc0000@ from the Tango color palette).
dullRed :: AnsiColour
dullRed =
    Escapes [SetRGBColor Foreground (sRGB24read "#CC0000")]

-- | Highlighted \"Scarlet Red\" (@#ef2929@ from the Tango color palette).
brightRed :: AnsiColour
brightRed =
    Escapes [SetRGBColor Foreground (sRGB24read "#EF2929")]

-- | Pure \"Red\" (full RGB red channel only).
pureRed :: AnsiColour
pureRed =
    Escapes [SetRGBColor Foreground (sRGB 1 0 0)]

-- | Shadowed \"Chameleon\" (@#4e9a06@ from the Tango color palette).
dullGreen :: AnsiColour
dullGreen =
    Escapes [SetRGBColor Foreground (sRGB24read "#4E9A06")]

-- | Highlighted \"Chameleon\" (@#8ae234@ from the Tango color palette).
brightGreen :: AnsiColour
brightGreen =
    Escapes [SetRGBColor Foreground (sRGB24read "#8AE234")]

-- | Pure \"Green\" (full RGB green channel only).
pureGreen :: AnsiColour
pureGreen =
    Escapes [SetRGBColor Foreground (sRGB 0 1 0)]

-- | Medium \"Sky Blue\" (@#3465a4@ from the Tango color palette).
dullBlue :: AnsiColour
dullBlue =
    Escapes [SetRGBColor Foreground (sRGB24read "#3465A4")]

-- | Highlighted \"Sky Blue\" (@#729fcf@ from the Tango color palette).
brightBlue :: AnsiColour
brightBlue =
    Escapes [SetRGBColor Foreground (sRGB24read "#729FCF")]

-- | Pure \"Blue\" (full RGB blue channel only).
pureBlue :: AnsiColour
pureBlue =
    Escapes [SetRGBColor Foreground (sRGB 0 0 1)]

-- | Dull \"Cyan\" (from the __gnome-terminal__ console theme).
dullCyan :: AnsiColour
dullCyan =
    Escapes [SetRGBColor Foreground (sRGB24read "#06989A")]

-- | Bright \"Cyan\" (from the __gnome-terminal__ console theme).
brightCyan :: AnsiColour
brightCyan =
    Escapes [SetRGBColor Foreground (sRGB24read "#34E2E2")]

-- | Pure \"Cyan\" (full RGB blue + green channels).
pureCyan :: AnsiColour
pureCyan =
    Escapes [SetRGBColor Foreground (sRGB 0 1 1)]

-- | Medium \"Plum\" (@#75507b@ from the Tango color palette).
dullMagenta :: AnsiColour
dullMagenta =
    Escapes [SetRGBColor Foreground (sRGB24read "#75507B")]

-- | Highlighted \"Plum\" (@#ad7fa8@ from the Tango color palette).
brightMagenta :: AnsiColour
brightMagenta =
    Escapes [SetRGBColor Foreground (sRGB24read "#AD7FA8")]

-- | Pure \"Magenta\" (full RGB red + blue channels).
pureMagenta :: AnsiColour
pureMagenta =
    Escapes [SetRGBColor Foreground (sRGB 1 0 1)]

-- | Shadowed \"Butter\" (@#c4a000@ from the Tango color palette).
dullYellow :: AnsiColour
dullYellow =
    Escapes [SetRGBColor Foreground (sRGB24read "#C4A000")]

-- | Highlighted \"Butter\" (@#fce94f@ from the Tango color palette).
brightYellow :: AnsiColour
brightYellow =
    Escapes [SetRGBColor Foreground (sRGB24read "#FCE94F")]

-- | Pure \"Yellow\" (full RGB red + green channels).
pureYellow :: AnsiColour
pureYellow =
    Escapes [SetRGBColor Foreground (sRGB 1 1 0)]

-- | Pure \"Black\" (zero in all RGB channels).
pureBlack :: AnsiColour
pureBlack =
    Escapes [SetRGBColor Foreground (sRGB 0 0 0)]

-- | Shadowed \"Deep Aluminium\" (@#2e3436@ from the Tango color palette).
dullGrey :: AnsiColour
dullGrey =
    Escapes [SetRGBColor Foreground (sRGB24read "#2E3436")]

-- | Medium \"Dark Aluminium\" (from the Tango color palette).
brightGrey :: AnsiColour
brightGrey =
    Escapes [SetRGBColor Foreground (sRGB24read "#555753")]

-- | Pure \"Grey\" (set at @#999999@, being just over half in all RGB channels).
pureGrey :: AnsiColour
pureGrey =
    Escapes [SetRGBColor Foreground (sRGB24read "#999999")]

-- | Pure \"White\" (fully on in all RGB channels).
pureWhite :: AnsiColour
pureWhite =
    Escapes [SetRGBColor Foreground (sRGB 1 1 1)]

-- | Medium \"Light Aluminium\" (@#d3d7cf@ from the Tango color palette).
dullWhite :: AnsiColour
dullWhite =
    Escapes [SetRGBColor Foreground (sRGB24read "#D3D7CF")]

-- | Highlighted \"Light Aluminium\" (@#eeeeec@ from the Tango color palette).
brightWhite :: AnsiColour
brightWhite =
    Escapes [SetRGBColor Foreground (sRGB24read "#EEEEEC")]

{- |
Given an 'AnsiColour', lift it to bold intensity.

Note that many console fonts do /not/ have a bold face variant, and terminal
emulators that "support bold" do so by doubling the thickness of the lines in
the glyphs. This may or may not be desirable from a readibility standpoint but
really there's only so much you can do to keep users who make poor font
choices from making poor font choices.
-}
boldColour :: AnsiColour -> AnsiColour
boldColour (Escapes list) =
    Escapes (SetConsoleIntensity BoldIntensity : list)

instance Semigroup AnsiColour where
    (<>) (Escapes list1) (Escapes list2) = Escapes (list1 <> list2)

instance Monoid AnsiColour where
    mempty = Escapes []

{- |
This is not a colour, obviously, but it represents reseting to the default
terminal foreground colour, whatever the user has that set to.
-}
resetColour :: AnsiColour
resetColour =
    Escapes [Reset]
