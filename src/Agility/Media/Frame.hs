-- | The decoded picture every media decoder produces and the renderer
-- consumes. A still image is one 'Frame'; a video will be a stream of them,
-- so nothing here knows about file formats or terminals.
module Agility.Media.Frame
  ( Frame (..),
    Rgba (..),
    mkFrame,
    pixelAt,
  )
where

import qualified Data.Vector.Storable as VS
import           Data.Word            (Word8)

-- | Width x height pixels, row-major, 4 bytes per pixel (R, G, B, A) with
-- straight (non-premultiplied) alpha. Storable so raw frames from an
-- external source can later be wrapped without copying.
data Frame = Frame
  { frameWidth  :: !Int,
    frameHeight :: !Int,
    framePixels :: !(VS.Vector Word8)
  }
  deriving (Eq)

instance Show Frame where
  show frame = "Frame " ++ show (frameWidth frame) ++ "x" ++ show (frameHeight frame)

data Rgba = Rgba !Word8 !Word8 !Word8 !Word8
  deriving (Eq, Show)

mkFrame :: Int -> Int -> VS.Vector Word8 -> Either String Frame
mkFrame width height pixels
  | width <= 0 || height <= 0 = Left "frame dimensions must be positive"
  | VS.length pixels /= width * height * 4 = Left "frame pixel buffer has the wrong size"
  | otherwise = Right (Frame width height pixels)

-- | Unchecked: callers must stay within the frame.
pixelAt :: Frame -> Int -> Int -> Rgba
pixelAt frame x y =
  let base = (y * frameWidth frame + x) * 4
      byte i = VS.unsafeIndex (framePixels frame) (base + i)
   in Rgba (byte 0) (byte 1) (byte 2) (byte 3)
{-# INLINE pixelAt #-}
