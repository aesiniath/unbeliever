{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK prune #-}

{- |
Machinery for generating identifiers to be used in traces and spans. Meets the
requirements of the [W3C Trace
Context](https://www.w3.org/TR/trace-context/#traceparent-header)
specification, specifically as relates to forming trace identifiers and span
identifiers into @traceparent@ headers. The key requirements are that traces
be globally unique and that spans be unique within a trace.
-}
module Core.Telemetry.Identifiers (
    -- * Traces and Spans
    getIdentifierTrace,
    getIdentifierSpan,

    -- * Internals
    createIdentifierTrace,
    createIdentifierSpan,
    hostMachineIdentity,
    createTraceParentHeader,
    -- for testing
    toHexNormal64,
    toHexReversed64,
    toHexNormal32,
    toHexReversed32,
) where

import Control.Concurrent.MVar (readMVar)
import Core.Program.Context
import Core.System (unsafePerformIO)
import Core.System.Base (liftIO)
import Core.System.External (TimeStamp (unTimeStamp))
import Core.Text.Rope
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Text.Internal.Unsafe.Char (unsafeChr8)
import GHC.Word
import Network.Info (MAC (..), NetworkInterface, getNetworkInterfaces, mac)

{- |
Get the MAC address of the first interface that's not the loopback device. If
something goes weird then we return a valid but bogus address (in the locally
administered addresses block).

@since 0.1.9
-}
hostMachineIdentity :: MAC
hostMachineIdentity = unsafePerformIO $ do
    interfaces <- getNetworkInterfaces
    pure (go interfaces)
  where
    go :: [NetworkInterface] -> MAC
    go [] = bogusAddress
    go (interface : remainder) =
        let address = mac interface
         in if address /= loopbackAddress
                then address
                else go remainder

    loopbackAddress = MAC 00 00 00 00 00 00
    bogusAddress = MAC 0xfe 0xff 0xff 0xff 0xff 0xff
{-# NOINLINE hostMachineIdentity #-}

{- |
Generate an identifier suitable for use in a trace context. Trace identifiers
are 16 bytes. We incorporate the time to nanosecond precision, the host
system's MAC address, and a random element. This is similar to a version 1
UUID, but we render the least significant bits of the time stamp ordered first
so that visual distinctiveness is on the left. The MAC address in the lower 48
bits is /not/ reversed, leaving the most distinctiveness [the actual host as
opposed to manufacturer OIN] hanging on the right hand edge of the identifier.
The two bytes of randomness are in the middle.

@since 0.1.9
-}
createIdentifierTrace :: TimeStamp -> Word16 -> MAC -> Trace
createIdentifierTrace time rand address =
    let p1 = packRope (toHexReversed64 (fromIntegral time))
        p2 = packRope (toHexNormal16 rand)
        p3 = packRope (convertMACToHex address)
     in Trace
            (p1 <> p2 <> p3)

convertMACToHex :: MAC -> [Char]
convertMACToHex (MAC b1 b2 b3 b4 b5 b6) =
    nibbleToHex b1 4 :
    nibbleToHex b1 0 :
    nibbleToHex b2 4 :
    nibbleToHex b2 0 :
    nibbleToHex b3 4 :
    nibbleToHex b3 0 :
    nibbleToHex b4 4 :
    nibbleToHex b4 0 :
    nibbleToHex b5 4 :
    nibbleToHex b5 0 :
    nibbleToHex b6 4 :
    nibbleToHex b6 0 :
    []
  where
    nibbleToHex w = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

toHexReversed64 :: Word64 -> [Char]
toHexReversed64 w =
    nibbleToHex 00 :
    nibbleToHex 04 :
    nibbleToHex 08 :
    nibbleToHex 12 :
    nibbleToHex 16 :
    nibbleToHex 20 :
    nibbleToHex 24 :
    nibbleToHex 28 : -- Word32
    nibbleToHex 32 :
    nibbleToHex 36 :
    nibbleToHex 40 :
    nibbleToHex 44 :
    nibbleToHex 48 :
    nibbleToHex 52 :
    nibbleToHex 56 :
    nibbleToHex 60 :
    []
  where
    nibbleToHex = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

toHexNormal64 :: Word64 -> [Char]
toHexNormal64 w =
    nibbleToHex 60 :
    nibbleToHex 56 :
    nibbleToHex 52 :
    nibbleToHex 48 :
    nibbleToHex 44 :
    nibbleToHex 40 :
    nibbleToHex 36 :
    nibbleToHex 32 :
    nibbleToHex 28 : -- Word32
    nibbleToHex 24 :
    nibbleToHex 20 :
    nibbleToHex 16 :
    nibbleToHex 12 :
    nibbleToHex 08 :
    nibbleToHex 04 :
    nibbleToHex 00 :
    []
  where
    nibbleToHex = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

--
-- Convert a 32-bit word to eight characters, but reversed so the least
-- significant bits are first.
--
toHexReversed32 :: Word32 -> [Char]
toHexReversed32 w =
    nibbleToHex 00 :
    nibbleToHex 04 :
    nibbleToHex 08 :
    nibbleToHex 12 :
    nibbleToHex 16 :
    nibbleToHex 20 :
    nibbleToHex 24 :
    nibbleToHex 28 :
    []
  where
    nibbleToHex = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

toHexNormal32 :: Word32 -> [Char]
toHexNormal32 w =
    nibbleToHex 28 :
    nibbleToHex 24 :
    nibbleToHex 20 :
    nibbleToHex 16 :
    nibbleToHex 12 :
    nibbleToHex 08 :
    nibbleToHex 04 :
    nibbleToHex 00 :
    []
  where
    nibbleToHex = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

toHexNormal16 :: Word16 -> [Char]
toHexNormal16 w =
    nibbleToHex 12 :
    nibbleToHex 08 :
    nibbleToHex 04 :
    nibbleToHex 00 :
    []
  where
    nibbleToHex = unsafeToDigit . fromIntegral . (.&.) 0x0f . shiftR w

{-
byteToHex :: Word8 -> [Char]
byteToHex c =
    let !low = unsafeToDigit (c .&. 0x0f)
        !hi = unsafeToDigit ((c .&. 0xf0) `shiftR` 4)
     in hi : low : []
-}

-- convert a nibble to its hexidecimal character equivalent
unsafeToDigit :: Word8 -> Char
unsafeToDigit w =
    if w < 10
        then unsafeChr8 (48 + w)
        else unsafeChr8 (97 + w - 10)

{- |
Generate an identifier for a span. We only have 8 bytes to work with. We use
the nanosecond prescision timestamp with the nibbles reversed, and then
overwrite the last two bytes with a random value.

@since 0.1.9
-}
createIdentifierSpan :: TimeStamp -> Word16 -> Span
createIdentifierSpan time rand =
    let t = fromIntegral (unTimeStamp time) :: Word64
        r = fromIntegral rand :: Word64
        w = (t .&. 0x0000ffffffffffff) .|. (shiftL r 48)
     in Span
            ( packRope
                ( toHexReversed64 w
                )
            )

{- |
Render the 'Trace' and 'Span' identifiers representing a span calling onward
to another component in a distributed system. The W3C Trace Context
recommendation specifies the HTTP header @traceparent@ with the 16 byte trace
identifier and the 8 byte span identifier formatted as follows:

@
traceparent: 00-fd533dbf96ecdc610156482ae36c24f7-1d1e9dbf96ec4649-00
@

@since 0.1.9
-}
createTraceParentHeader :: Trace -> Span -> Rope
createTraceParentHeader trace unique =
    let version = "00"
        flags = "00"
     in version <> "-" <> unTrace trace <> "-" <> unSpan unique <> "-" <> flags

{- |
Get the identifier of the current trace, if you are ithin a trace started by
'beginTrace' or 'usingTrace'.

@since 0.1.9
-}
getIdentifierTrace :: Program τ (Maybe Trace)
getIdentifierTrace = do
    context <- getContext

    liftIO $ do
        let v = currentDatumFrom context
        datum <- readMVar v

        pure (traceIdentifierFrom datum)

{- |
Get the identifier of the current span, if you are currently within a span
created by 'encloseSpan'.

@since 0.1.9
-}
getIdentifierSpan :: Program τ (Maybe Span)
getIdentifierSpan = do
    context <- getContext

    liftIO $ do
        let v = currentDatumFrom context
        datum <- readMVar v

        pure (spanIdentifierFrom datum)
