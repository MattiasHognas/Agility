-- | Turns frames into terminal cells. Each cell shows two vertically stacked
-- pixels using half-block characters: the upper pixel as the foreground of
-- '▀' and the lower one as its background. Transparent pixels leave the
-- terminal's own background visible.
module Agility.Media.Render
  ( renderFrame,
    frameToImage,
  )
where

import           Agility.Media.Frame (Frame (..), Rgba (..), pixelAt)
import           Agility.Media.Scale (fitWithin, scaleFrame)
import           Data.List           (groupBy)
import           Data.Function       (on)
import qualified Graphics.Vty        as V

-- | Fit a frame into a cols x rows cell area, keeping its aspect ratio.
renderFrame :: Int -> Int -> Frame -> V.Image
renderFrame cols rows frame =
  let (w, h) = fitWithin (frameWidth frame) (frameHeight frame) cols (rows * 2)
   in frameToImage (scaleFrame w h frame)

-- | One cell per pixel column and per two pixel rows, without scaling.
frameToImage :: Frame -> V.Image
frameToImage frame =
  V.vertCat [cellRow y | y <- [0, 2 .. frameHeight frame - 1]]
  where
    cellRow y =
      V.horizCat
        [ V.string attr (map snd run)
          | run@((attr, _) : _) <- groupBy ((==) `on` fst) [cell x y | x <- [0 .. frameWidth frame - 1]]
        ]
    cell x y =
      let top = visible (pixelAt frame x y)
          bottom =
            if y + 1 < frameHeight frame
              then visible (pixelAt frame x (y + 1))
              else Nothing
       in case (top, bottom) of
            (Just t, Just b) -> (V.defAttr `V.withForeColor` t `V.withBackColor` b, '▀')
            (Just t, Nothing) -> (V.defAttr `V.withForeColor` t, '▀')
            (Nothing, Just b) -> (V.defAttr `V.withForeColor` b, '▄')
            (Nothing, Nothing) -> (V.defAttr, ' ')

-- | Pixels at least half opaque are drawn; the rest are left transparent.
visible :: Rgba -> Maybe V.Color
visible (Rgba r g b a)
  | a >= 128 = Just (V.linearColor r g b)
  | otherwise = Nothing
