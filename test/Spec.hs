module Main (main) where

import           Agility.Media          (decodeImage)
import           Agility.Media.Frame    (Frame (..), Rgba (..), mkFrame,
                                         pixelAt)
import           Agility.Media.Render   (frameToImage, renderFrame)
import           Agility.Media.Scale    (fitWithin, scaleFrame)
import           Control.Monad          (unless)
import           Data.Bits              (xor)
import qualified Data.ByteString        as B
import           Data.IORef             (IORef, modifyIORef', newIORef,
                                         readIORef)
import qualified Data.Vector.Storable   as VS
import           Data.Word              (Word64, Word8)
import qualified Graphics.Vty           as V
import           Numeric                (showHex)
import           System.Exit            (exitFailure)

main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  pngSuite failures
  unitTests failures
  failed <- readIORef failures
  if failed == 0
    then putStrLn "All tests passed."
    else do
      putStrLn (show failed ++ " test(s) failed.")
      exitFailure

check :: IORef Int -> String -> Bool -> IO ()
check failures name ok =
  unless ok $ do
    putStrLn ("FAIL: " ++ name)
    modifyIORef' failures (+ 1)

-- | Every image in PngSuite must decode to the same RGBA pixels as the
-- reference decoder (pypng), and every deliberately corrupt image (x*.png)
-- must be rejected. expected.txt holds "name width height fnv1a64" or
-- "name error" per file.
pngSuite :: IORef Int -> IO ()
pngSuite failures = do
  let dir = "test/fixtures/pngsuite"
  expected <- lines <$> readFile (dir </> "expected.txt")
  mapM_ (checkFile dir) (map words expected)
  putStrLn ("PngSuite: checked " ++ show (length expected) ++ " images.")
  where
    checkFile dir [name, "error"] = do
      bytes <- B.readFile (dir </> name)
      case decodeImage bytes of
        Left _  -> pure ()
        Right _ -> check failures (name ++ ": corrupt image was accepted") False
    checkFile dir [name, w, h, hash] = do
      bytes <- B.readFile (dir </> name)
      case decodeImage bytes of
        Left err -> check failures (name ++ ": " ++ err) False
        Right frame -> do
          check failures (name ++ ": wrong size") (show (frameWidth frame) == w && show (frameHeight frame) == h)
          check failures (name ++ ": wrong pixels") (padHex (fnv1a (framePixels frame)) == hash)
    checkFile _ other = check failures ("bad expected.txt line: " ++ unwords other) False

    padHex n = let s = showHex n "" in replicate (16 - length s) '0' ++ s

(</>) :: FilePath -> FilePath -> FilePath
dir </> name = dir ++ "/" ++ name

fnv1a :: VS.Vector Word8 -> Word64
fnv1a = VS.foldl' (\h b -> (h `xor` fromIntegral b) * 0x100000001b3) 0xcbf29ce484222325

unitTests :: IORef Int -> IO ()
unitTests failures = do
  check failures "non-PNG input is rejected" $
    isLeft (decodeImage (B.pack [0x47, 0x49, 0x46, 0x38, 0x39, 0x61]))
  check failures "empty input is rejected" $
    isLeft (decodeImage B.empty)

  pngBytes <- B.readFile "test/fixtures/pngsuite/basn6a08.png"
  check failures "truncated PNG is rejected" $
    isLeft (decodeImage (B.take (B.length pngBytes - 20) pngBytes))
  check failures "flipped data byte fails the CRC check" $
    isLeft (decodeImage (flipByte 60 pngBytes))

  check failures "fitWithin keeps aspect ratio when width-bound" $
    fitWithin 200 100 50 50 == (50, 25)
  check failures "fitWithin keeps aspect ratio when height-bound" $
    fitWithin 100 200 50 50 == (25, 50)
  check failures "fitWithin enlarges small images" $
    fitWithin 10 10 40 20 == (20, 20)

  -- An opaque blue pixel next to a fully transparent red one averages to
  -- half-transparent blue: the hidden red must not bleed in.
  let pair = frameOf 2 1 (VS.fromList [255, 0, 0, 0, 0, 0, 255, 255])
      merged = scaleFrame 1 1 pair
  check failures "scaling ignores the colour of transparent pixels" $
    pixelAt merged 0 0 == Rgba 0 0 255 128

  let solid = frameOf 4 4 (VS.fromList (concat (replicate 16 [10, 20, 30, 255])))
  check failures "scaling a uniform image keeps its colour" $
    all (== Rgba 10 20 30 255) [pixelAt (scaleFrame 2 2 solid) x y | x <- [0, 1], y <- [0, 1]]

  let tall = frameOf 3 5 (VS.replicate (3 * 5 * 4) 255)
      tallImage = frameToImage tall
  check failures "two pixel rows per terminal row, rounding up" $
    V.imageWidth tallImage == 3 && V.imageHeight tallImage == 3

  let fitted = renderFrame 40 10 solid
  check failures "renderFrame fits inside the cell area" $
    V.imageWidth fitted <= 40 && V.imageHeight fitted <= 10
  where
    frameOf w h pixels = either error id (mkFrame w h pixels)
    isLeft = either (const True) (const False)
    flipByte i bytes =
      B.take i bytes <> B.singleton (B.index bytes i `xor` 0xff) <> B.drop (i + 1) bytes
