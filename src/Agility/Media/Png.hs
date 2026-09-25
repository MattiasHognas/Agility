{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

-- | PNG decoder producing an RGBA 'Frame'.
--
-- Supports every colour type and bit depth in the PNG specification, all
-- five scanline filters, Adam7 interlacing and tRNS transparency. Ancillary
-- chunks other than tRNS (gamma, colour profiles, text, ...) are skipped.
-- Chunks are parsed into a list first so animated PNG (acTL/fcTL/fdAT) can
-- be layered on top later.
module Agility.Media.Png
  ( decodePng,
    isPng,
  )
where

import           Agility.Media.Frame                (Frame (..))
import           Control.Monad                      (unless, when)
import           Control.Monad.ST                   (ST)
import           Data.Bits                          (complement, shiftL,
                                                     shiftR, xor, (.&.), (.|.))
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy               as L
import qualified Data.ByteString.Unsafe             as BU
import qualified Codec.Compression.Zlib.Internal    as Z
import           Data.Char                          (isAsciiLower,
                                                     isAsciiUpper)
import qualified Data.ByteString.Char8              as BC
import qualified Data.Vector.Storable               as VS
import qualified Data.Vector.Storable.Mutable       as MVS
import qualified Data.Vector.Unboxed                as VU
import           Data.Word                          (Word32, Word8)

data ColorType
  = Grayscale
  | Truecolor
  | Indexed
  | GrayscaleAlpha
  | TruecolorAlpha
  deriving (Eq, Show)

data Header = Header
  { hdrWidth      :: !Int,
    hdrHeight     :: !Int,
    hdrBitDepth   :: !Int,
    hdrColorType  :: !ColorType,
    hdrInterlaced :: !Bool
  }

data Chunk = Chunk
  { chunkType :: !B.ByteString,
    chunkData :: !B.ByteString
  }

-- | How transparency is expressed, resolved from tRNS for the colour type.
data Transparency
  = NoTransparency
  | TransparentGray !Int
  | TransparentRgb !Int !Int !Int

-- | Guards against decompression bombs and absurd headers.
maxPixels :: Int
maxPixels = 64 * 1024 * 1024

pngSignature :: B.ByteString
pngSignature = B.pack [137, 80, 78, 71, 13, 10, 26, 10]

isPng :: B.ByteString -> Bool
isPng = B.isPrefixOf pngSignature

decodePng :: B.ByteString -> Either String Frame
decodePng bytes = do
  chunks <- parseChunks bytes
  (header, rest) <- case chunks of
    Chunk "IHDR" body : more -> (,more) <$> parseHeader body
    _                        -> Left "PNG must start with an IHDR chunk"
  mapM_ rejectUnknownCritical rest
  let bodiesOf typ = [chunkData c | c <- rest, chunkType c == typ]
      colorType = hdrColorType header
  palette <- case bodiesOf "PLTE" of
    [] | colorType == Indexed -> Left "indexed PNG is missing its PLTE chunk"
    []                        -> Right Nothing
    [body]                    -> Just <$> parsePalette body
    _                         -> Left "PNG has more than one PLTE chunk"
  let trns = case bodiesOf "tRNS" of
        body : _ -> Just body
        []       -> Nothing
      idats = bodiesOf "IDAT"
  when (null idats) $ Left "PNG has no image data (IDAT)"
  raw <- inflateExactly (expectedDataSize header) (L.fromChunks idats)
  validateFilterTypes header raw
  let paletteTable = buildPaletteTable palette trns
      transparency = resolveTransparency header trns
  pure $! reconstruct header paletteTable transparency raw

-- Chunks ---------------------------------------------------------------------

parseChunks :: B.ByteString -> Either String [Chunk]
parseChunks bytes
  | not (isPng bytes) = Left "not a PNG file (bad signature)"
  | otherwise = go (B.drop 8 bytes)
  where
    go rest
      | B.length rest < 12 = Left "truncated PNG (no IEND chunk)"
      | otherwise = do
          let len = fromIntegral (readWord32 rest 0)
              typ = B.take 4 (B.drop 4 rest)
          unless (BC.all (\c -> isAsciiUpper c || isAsciiLower c) typ) $
            Left "corrupt PNG (invalid chunk type)"
          when (len > 0x7fffffff || B.length rest < 12 + len) $
            Left ("truncated PNG (" ++ BC.unpack typ ++ " chunk)")
          let expectedCrc = readWord32 rest (8 + len)
          unless (crc32 (B.take (4 + len) (B.drop 4 rest)) == expectedCrc) $
            Left ("corrupt PNG (CRC mismatch in " ++ BC.unpack typ ++ " chunk)")
          let chunk = Chunk typ (B.take len (B.drop 8 rest))
          if typ == "IEND"
            then Right [chunk]
            else (chunk :) <$> go (B.drop (12 + len) rest)

-- | Critical chunks start with an upper-case letter; a decoder must refuse
-- ones it does not understand.
rejectUnknownCritical :: Chunk -> Either String ()
rejectUnknownCritical (Chunk typ _)
  | typ `elem` ["IHDR", "PLTE", "IDAT", "IEND"] = Right ()
  | isAsciiUpper (BC.head typ) = Left ("unsupported critical PNG chunk " ++ BC.unpack typ)
  | otherwise = Right ()

parseHeader :: B.ByteString -> Either String Header
parseHeader body = do
  when (B.length body /= 13) $ Left "corrupt PNG (IHDR has the wrong length)"
  let width = fromIntegral (readWord32 body 0)
      height = fromIntegral (readWord32 body 4)
      depth = fromIntegral (B.index body 8)
      byteAt i = B.index body i
  when (width <= 0 || height <= 0) $ Left "PNG has zero width or height"
  when (width > maxPixels || height > maxPixels || width * height > maxPixels) $
    Left "PNG is too large"
  colorType <- case byteAt 9 of
    0 -> Right Grayscale
    2 -> Right Truecolor
    3 -> Right Indexed
    4 -> Right GrayscaleAlpha
    6 -> Right TruecolorAlpha
    n -> Left ("invalid PNG colour type " ++ show n)
  let allowedDepths = case colorType of
        Grayscale -> [1, 2, 4, 8, 16]
        Indexed   -> [1, 2, 4, 8]
        _         -> [8, 16]
  unless (depth `elem` allowedDepths) $
    Left ("invalid PNG bit depth " ++ show depth ++ " for " ++ show colorType)
  unless (byteAt 10 == 0) $ Left "unsupported PNG compression method"
  unless (byteAt 11 == 0) $ Left "unsupported PNG filter method"
  interlaced <- case byteAt 12 of
    0 -> Right False
    1 -> Right True
    n -> Left ("invalid PNG interlace method " ++ show n)
  pure (Header width height depth colorType interlaced)

parsePalette :: B.ByteString -> Either String B.ByteString
parsePalette body
  | B.null body || B.length body `mod` 3 /= 0 || B.length body > 256 * 3 =
      Left "corrupt PNG (invalid PLTE chunk)"
  | otherwise = Right body

-- | 256 RGBA entries; indices past the palette decode as opaque black.
buildPaletteTable :: Maybe B.ByteString -> Maybe B.ByteString -> VU.Vector Word8
buildPaletteTable Nothing _ = VU.empty
buildPaletteTable (Just plte) trns =
  VU.generate (256 * 4) $ \i ->
    let (entry, channel) = i `divMod` 4
        entries = B.length plte `div` 3
     in case channel of
          3 -> case trns of
            Just alpha | entry < B.length alpha -> B.index alpha entry
            _                                    -> 255
          _
            | entry < entries -> B.index plte (entry * 3 + channel)
            | otherwise -> 0

resolveTransparency :: Header -> Maybe B.ByteString -> Transparency
resolveTransparency header (Just body) =
  case hdrColorType header of
    Grayscale | B.length body >= 2 -> TransparentGray (sample16 0)
    Truecolor
      | B.length body >= 6 ->
          TransparentRgb (sample16 0) (sample16 2) (sample16 4)
    _ -> NoTransparency
  where
    -- Only the low bits matter for bit depths below 16.
    sample16 i =
      (fromIntegral (B.index body i) `shiftL` 8 .|. fromIntegral (B.index body (i + 1)))
        .&. ((1 `shiftL` hdrBitDepth header) - 1)
resolveTransparency _ Nothing = NoTransparency

-- Decompression --------------------------------------------------------------

-- | Inflate the zlib stream, stopping as soon as the expected number of bytes
-- has been produced so a malicious stream cannot exhaust memory.
inflateExactly :: Int -> L.ByteString -> Either String B.ByteString
inflateExactly expected input =
  fmap (B.take expected . B.concat) (run expected)
  where
    params = Z.defaultDecompressParams {Z.decompressBufferSize = max 4096 (min expected (1024 * 1024))}
    run =
      Z.foldDecompressStreamWithInput
        (\chunk continue needed ->
            if needed <= 0
              then Right []
              else (chunk :) <$> continue (needed - B.length chunk))
        (\_ needed ->
            if needed > 0
              then Left "corrupt PNG (image data is truncated)"
              else Right [])
        (\err _ -> Left ("corrupt PNG (" ++ show err ++ ")"))
        (Z.decompressST Z.zlibFormat params)
        input

-- Scanlines ------------------------------------------------------------------

-- | (x start, y start, x step, y step) of each Adam7 pass.
adam7 :: [(Int, Int, Int, Int)]
adam7 =
  [ (0, 0, 8, 8),
    (4, 0, 8, 8),
    (0, 4, 4, 8),
    (2, 0, 4, 4),
    (0, 2, 2, 4),
    (1, 0, 2, 2),
    (0, 1, 1, 2)
  ]

data Pass = Pass
  { passX0     :: !Int,
    passY0     :: !Int,
    passDx     :: !Int,
    passDy     :: !Int,
    passWidth  :: !Int,
    passHeight :: !Int,
    passOffset :: !Int -- ^ Where the pass's filtered rows start in the raw data.
  }

passes :: Header -> [Pass]
passes header = go 0 layout
  where
    layout
      | hdrInterlaced header = adam7
      | otherwise = [(0, 0, 1, 1)]
    go _ [] = []
    go offset ((x0, y0, dx, dy) : rest) =
      let w = (hdrWidth header - x0 + dx - 1) `div` dx
          h = (hdrHeight header - y0 + dy - 1) `div` dy
          size = if w == 0 || h == 0 then 0 else h * (1 + rowBytes header w)
          pass = Pass x0 y0 dx dy w h offset
       in if size == 0 then go offset rest else pass : go (offset + size) rest

bitsPerPixel :: Header -> Int
bitsPerPixel header = hdrBitDepth header * channelCount (hdrColorType header)

channelCount :: ColorType -> Int
channelCount colorType = case colorType of
  Grayscale      -> 1
  Truecolor      -> 3
  Indexed        -> 1
  GrayscaleAlpha -> 2
  TruecolorAlpha -> 4

rowBytes :: Header -> Int -> Int
rowBytes header width = (width * bitsPerPixel header + 7) `div` 8

expectedDataSize :: Header -> Int
expectedDataSize header =
  sum [passHeight p * (1 + rowBytes header (passWidth p)) | p <- passes header]

validateFilterTypes :: Header -> B.ByteString -> Either String ()
validateFilterTypes header raw =
  unless (all valid (passes header)) $ Left "corrupt PNG (invalid scanline filter)"
  where
    valid p =
      let stride = 1 + rowBytes header (passWidth p)
       in all (\y -> BU.unsafeIndex raw (passOffset p + y * stride) <= 4) [0 .. passHeight p - 1]

-- | Undo the scanline filters of one pass into a buffer of plain rows.
unfilterPass :: Header -> B.ByteString -> Pass -> ST s (MVS.MVector s Word8)
unfilterPass header raw p = do
  let stride = rowBytes header (passWidth p)
      bpp = max 1 (bitsPerPixel header `div` 8)
  out <- MVS.new (passHeight p * stride)
  loop (passHeight p) $ \y -> do
    let src = passOffset p + y * (stride + 1)
        dst = y * stride
        filtered x = BU.unsafeIndex raw (src + 1 + x)
        left x = if x >= bpp then MVS.unsafeRead out (dst + x - bpp) else pure 0
        up x = if y > 0 then MVS.unsafeRead out (dst - stride + x) else pure 0
        upLeft x =
          if y > 0 && x >= bpp then MVS.unsafeRead out (dst - stride + x - bpp) else pure 0
        emit f = loop stride $ \x -> f x >>= MVS.unsafeWrite out (dst + x) . (filtered x +)
        {-# INLINE emit #-}
    case BU.unsafeIndex raw src of
      0 -> emit (const (pure 0))
      1 -> emit left
      2 -> emit up
      3 -> emit $ \x -> do
        a <- left x
        b <- up x
        pure (fromIntegral ((toInt a + toInt b) `div` 2))
      _ -> emit $ \x -> paeth <$> left x <*> up x <*> upLeft x
  pure out

paeth :: Word8 -> Word8 -> Word8 -> Word8
paeth a b c =
  let p = toInt a + toInt b - toInt c
      pa = abs (p - toInt a)
      pb = abs (p - toInt b)
      pc = abs (p - toInt c)
   in if pa <= pb && pa <= pc then a else if pb <= pc then b else c
{-# INLINE paeth #-}

-- Pixels ---------------------------------------------------------------------

reconstruct :: Header -> VU.Vector Word8 -> Transparency -> B.ByteString -> Frame
reconstruct header paletteTable transparency raw =
  Frame width (hdrHeight header) $ VS.create $ do
    pixels <- MVS.replicate (width * hdrHeight header * 4) 0
    mapM_ (decodePass pixels) (passes header)
    pure pixels
  where
    width = hdrWidth header
    depth = hdrBitDepth header
    colorType = hdrColorType header
    channels = channelCount colorType
    maxSample = (1 `shiftL` depth) - 1 :: Int

    decodePass pixels p = do
      rows <- unfilterPass header raw p
      let stride = rowBytes header (passWidth p)
          -- Each case below gets its own copy of this loop, so the colour
          -- type and bit depth are inspected once per pass, not per pixel.
          eachPixel body =
            loop (passHeight p) $ \j -> do
              let rowStart = j * stride
                  rowTarget = (passY0 p + j * passDy p) * width + passX0 p
              loop (passWidth p) $ \i ->
                body rowStart i ((rowTarget + i * passDx p) * 4)
          {-# INLINE eachPixel #-}
      convertPixels rows pixels eachPixel

    -- The common 8-bit layouts read bytes directly.
    convertPixels rows pixels eachPixel = case (colorType, depth, transparency) of
      (TruecolorAlpha, 8, _) -> eachPixel $ \rowStart i target -> do
        let src = rowStart + i * 4
        copyByte target src
        copyByte (target + 1) (src + 1)
        copyByte (target + 2) (src + 2)
        copyByte (target + 3) (src + 3)
      (Truecolor, 8, NoTransparency) -> eachPixel $ \rowStart i target -> do
        let src = rowStart + i * 3
        copyByte target src
        copyByte (target + 1) (src + 1)
        copyByte (target + 2) (src + 2)
        MVS.unsafeWrite pixels (target + 3) 255
      (Indexed, 8, _) -> eachPixel $ \rowStart i target -> do
        index <- toInt <$> MVS.unsafeRead rows (rowStart + i)
        putPalette target index
      (Indexed, _, _) -> eachPixel $ \rowStart i target -> do
        index <- sampleAt rowStart i 0
        putPalette target index
      (Grayscale, _, _) -> eachPixel $ \rowStart i target -> do
        v <- sampleAt rowStart i 0
        let alpha = case transparency of
              TransparentGray t | t == v -> 0
              _                          -> 255
            g = to8 v
        put target g g g alpha
      (Truecolor, _, _) -> eachPixel $ \rowStart i target -> do
        r <- sampleAt rowStart i 0
        g <- sampleAt rowStart i 1
        b <- sampleAt rowStart i 2
        let alpha = case transparency of
              TransparentRgb tr tg tb | tr == r && tg == g && tb == b -> 0
              _ -> 255
        put target (to8 r) (to8 g) (to8 b) alpha
      (GrayscaleAlpha, _, _) -> eachPixel $ \rowStart i target -> do
        v <- sampleAt rowStart i 0
        a <- sampleAt rowStart i 1
        let g = to8 v
        put target g g g (to8 a)
      (TruecolorAlpha, _, _) -> eachPixel $ \rowStart i target -> do
        r <- sampleAt rowStart i 0
        g <- sampleAt rowStart i 1
        b <- sampleAt rowStart i 2
        a <- sampleAt rowStart i 3
        put target (to8 r) (to8 g) (to8 b) (to8 a)
      where
        sampleAt rowStart i k = readSample rows rowStart (i * channels + k)
        copyByte target src = MVS.unsafeRead rows src >>= MVS.unsafeWrite pixels target
        putPalette target index = do
          let entry k = VU.unsafeIndex paletteTable (index * 4 + k)
          put target (entry 0) (entry 1) (entry 2) (entry 3)
        put target r g b a = do
          MVS.unsafeWrite pixels target r
          MVS.unsafeWrite pixels (target + 1) g
          MVS.unsafeWrite pixels (target + 2) b
          MVS.unsafeWrite pixels (target + 3) a

    -- Sample k of a row, at the image's bit depth.
    readSample rows rowStart k = case depth of
      8 -> toInt <$> MVS.unsafeRead rows (rowStart + k)
      16 -> do
        hi <- MVS.unsafeRead rows (rowStart + 2 * k)
        lo <- MVS.unsafeRead rows (rowStart + 2 * k + 1)
        pure (toInt hi `shiftL` 8 .|. toInt lo)
      _ -> do
        let bit = k * depth
        byte <- MVS.unsafeRead rows (rowStart + bit `shiftR` 3)
        pure ((toInt byte `shiftR` (8 - depth - (bit .&. 7))) .&. maxSample)

    -- Rescale a sample to 8 bits, rounding to nearest.
    to8 :: Int -> Word8
    to8 v = case depth of
      8  -> fromIntegral v
      16 -> fromIntegral ((v * 2 + 257) `div` 514)
      _  -> fromIntegral (v * 255 `div` maxSample)

-- Helpers --------------------------------------------------------------------

loop :: Int -> (Int -> ST s ()) -> ST s ()
loop n body = go 0
  where
    go i
      | i >= n = pure ()
      | otherwise = body i >> go (i + 1)
{-# INLINE loop #-}

toInt :: Word8 -> Int
toInt = fromIntegral
{-# INLINE toInt #-}

readWord32 :: B.ByteString -> Int -> Word32
readWord32 bytes i =
  (byte 0 `shiftL` 24) .|. (byte 1 `shiftL` 16) .|. (byte 2 `shiftL` 8) .|. byte 3
  where
    byte k = fromIntegral (BU.unsafeIndex bytes (i + k))

crcTable :: VU.Vector Word32
crcTable = VU.generate 256 (step 8 . fromIntegral)
  where
    step :: Int -> Word32 -> Word32
    step 0 c = c
    step k c = step (k - 1) (if c .&. 1 == 1 then 0xedb88320 `xor` (c `shiftR` 1) else c `shiftR` 1)

crc32 :: B.ByteString -> Word32
crc32 = complement . B.foldl' update 0xffffffff
  where
    update c byte = VU.unsafeIndex crcTable (fromIntegral ((c `xor` fromIntegral byte) .&. 0xff)) `xor` (c `shiftR` 8)
