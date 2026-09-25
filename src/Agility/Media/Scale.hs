-- | Resizing frames for display.
module Agility.Media.Scale
  ( fitWithin,
    limitFrame,
    scaleFrame,
  )
where

import           Agility.Media.Frame          (Frame (..))
import           Control.Monad.ST             (ST)
import qualified Data.Vector.Storable         as VS
import qualified Data.Vector.Storable.Mutable as MVS
import qualified Data.Vector.Unboxed          as VU
import qualified Data.Vector.Unboxed.Mutable  as MVU
import           Data.Word                    (Word8)

-- | The largest size with the source's aspect ratio that fits the box.
fitWithin :: Int -> Int -> Int -> Int -> (Int, Int)
fitWithin srcWidth srcHeight boxWidth boxHeight =
  let scale =
        min
          (fromIntegral (max 1 boxWidth) / fromIntegral srcWidth :: Double)
          (fromIntegral (max 1 boxHeight) / fromIntegral srcHeight)
      fit src box = max 1 (min box (round (fromIntegral src * scale)))
   in (fit srcWidth (max 1 boxWidth), fit srcHeight (max 1 boxHeight))

-- | Shrink a frame to fit the box, leaving smaller frames untouched. Used
-- once after decoding so per-draw scaling works on a small frame.
limitFrame :: Int -> Int -> Frame -> Frame
limitFrame maxWidth maxHeight frame
  | frameWidth frame <= maxWidth && frameHeight frame <= maxHeight = frame
  | otherwise =
      let (w, h) = fitWithin (frameWidth frame) (frameHeight frame) maxWidth maxHeight
       in scaleFrame w h frame

-- | Resample to exactly the given size. Each target pixel averages the
-- source pixels it covers, weighting colour by alpha so transparent pixels
-- do not darken edges. Enlarging repeats pixels.
scaleFrame :: Int -> Int -> Frame -> Frame
scaleFrame targetWidth targetHeight frame
  | w == srcWidth && h == srcHeight = frame
  | otherwise = Frame w h (VS.create (MVS.new (w * h * 4) >>= fill))
  where
    w = max 1 targetWidth
    h = max 1 targetHeight
    srcWidth = frameWidth frame
    srcHeight = frameHeight frame
    src = framePixels frame
    byte i = fromIntegral (VS.unsafeIndex src i) :: Int

    -- Source range [lo, hi) covered by target index t.
    spanStart t target source = t * source `div` target
    spanEnd t target source =
      min source (max (spanStart t target source + 1) ((t + 1) * source `div` target))
    xStarts = VU.generate w (\t -> spanStart t w srcWidth)
    xEnds = VU.generate w (\t -> spanEnd t w srcWidth)

    fill :: MVS.MVector s Word8 -> ST s (MVS.MVector s Word8)
    fill out = do
      -- Alpha-weighted R, G, B sums and the alpha sum for one target row.
      sums <- MVU.new (w * 4)
      loop h $ \ty -> do
        MVU.set sums 0
        let y0 = spanStart ty h srcHeight
            y1 = spanEnd ty h srcHeight
        forRange y0 y1 $ \y -> loop w $ \tx -> do
          let accumulate x !r !g !b !a
                | x >= VU.unsafeIndex xEnds tx = do
                    add (tx * 4) r
                    add (tx * 4 + 1) g
                    add (tx * 4 + 2) b
                    add (tx * 4 + 3) a
                | otherwise =
                    let i = (y * srcWidth + x) * 4
                        alpha = byte (i + 3)
                     in accumulate
                          (x + 1)
                          (r + byte i * alpha)
                          (g + byte (i + 1) * alpha)
                          (b + byte (i + 2) * alpha)
                          (a + alpha)
              add i v = MVU.unsafeModify sums (+ v) i
          accumulate (VU.unsafeIndex xStarts tx) 0 0 0 0
        loop w $ \tx -> do
          let count = (VU.unsafeIndex xEnds tx - VU.unsafeIndex xStarts tx) * (y1 - y0)
              target = (ty * w + tx) * 4
          sumA <- MVU.unsafeRead sums (tx * 4 + 3)
          let channel k = do
                s <- MVU.unsafeRead sums (tx * 4 + k)
                MVS.unsafeWrite out (target + k) $
                  if sumA == 0 then 0 else fromIntegral ((s + sumA `div` 2) `div` sumA)
          channel 0
          channel 1
          channel 2
          MVS.unsafeWrite out (target + 3) (fromIntegral ((sumA + count `div` 2) `div` count))
      pure out

forRange :: Int -> Int -> (Int -> ST s ()) -> ST s ()
forRange from to body = go from
  where
    go i
      | i >= to = pure ()
      | otherwise = body i >> go (i + 1)
{-# INLINE forRange #-}

loop :: Int -> (Int -> ST s ()) -> ST s ()
loop n body = go 0
  where
    go i
      | i >= n = pure ()
      | otherwise = body i >> go (i + 1)
{-# INLINE loop #-}
