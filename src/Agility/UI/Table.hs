module Agility.UI.Table
  ( drawTable,
  )
where

import           Agility.Color        (borderAttr, headerAttr, linkAttr,
                                       selectedTableAttr, tablePagingAttr,
                                       tableTitleAttr, textAttr)
import           Agility.Dashboard    (distributeWidths)
import           Agility.State        (clampIndex, safeIndex)
import           Agility.Types        (Name (..), Row,
                                       St (activeTableIndex, colPositions, pagePositions, rowPositions),
                                       TableConfig (columnHeaders, columnWeights, maxColumnHeight, minColumnHeight, source, title),
                                       TableSource (ImageSource))
import           Brick                (Context (availHeight, availWidth),
                                       Padding (Max, Pad),
                                       Size (Fixed), Widget (Widget, render),
                                       clickable, emptyWidget, getContext,
                                       hBox, padLeft, padRight, padTop, str,
                                       raw, reportExtent, vBox, withAttr,
                                       (<+>))
import           Brick.Widgets.Border (border)
import           Codec.Picture        (DynamicImage, Image, PixelRGB8 (..),
                                       convertRGB8, decodeImage, generateImage,
                                       imageHeight, imageWidth, pixelAt)
import           Control.Exception    (IOException, SomeAsyncException, catch,
                                       fromException, throwIO, try)
import           Data.Char            (isDigit)
import qualified Data.ByteString.Lazy as B
import           Data.IORef           (IORef, atomicModifyIORef', newIORef,
                                       readIORef)
import           Data.List            (transpose, zipWith4)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe, isJust)
import           Data.Time.Clock      (UTCTime, addUTCTime, getCurrentTime)
import qualified Data.Text            as T
import           Network.HTTP.Simple  (getResponseBody, httpLBS, parseRequest)
import           System.Directory     (getTemporaryDirectory, removeFile)
import           System.Exit          (ExitCode (ExitSuccess))
import           System.IO            (hClose, openTempFile)
import           System.IO.Unsafe     (unsafePerformIO)
import           System.Process       (readProcessWithExitCode)
import qualified Graphics.Vty         as V
import qualified Graphics.Vty.Image   as VI

wrapOrTruncate :: Int -> Int -> String -> [String]
wrapOrTruncate width maxHeight txt =
  let chunks = map T.unpack $ T.chunksOf width (T.pack txt)
   in if length chunks <= maxHeight
        then chunks
        else
          let prefix = take (maxHeight - 1) chunks
           in case drop (maxHeight - 1) chunks of
                chunk : _ -> prefix ++ [take (width - 3) chunk ++ "..."]
                []        -> prefix

padCells :: Int -> [[String]] -> [[String]]
padCells height = map (\xs -> xs ++ replicate (height - length xs) "")

type ImageCacheKey = (String, Int, Int)

type CachedImage = (UTCTime, V.Image)

imageCache :: IORef (M.Map ImageCacheKey CachedImage)
imageCache = unsafePerformIO (newIORef M.empty)
{-# NOINLINE imageCache #-}

renderImage :: String -> Int -> Int -> Int -> V.Image
renderImage imageUrl refreshSeconds width height =
  unsafePerformIO $ do
    now <- getCurrentTime
    cache <- readIORef imageCache
    let key = (imageUrl, width, height)
    case M.lookup key cache of
      Just (_, cached)
        | refreshSeconds == 0 -> pure cached
      Just (cachedAt, cached)
        | now < addUTCTime (fromIntegral refreshSeconds) cachedAt -> pure cached
      Nothing -> do
        rendered <- loadImage imageUrl width height
        atomicModifyIORef' imageCache $ \current ->
          (M.insert key (now, rendered) current, ())
        pure rendered
      Just _ -> do
        rendered <- loadImage imageUrl width height
        atomicModifyIORef' imageCache $ \current ->
          (M.insert key (now, rendered) current, ())
        pure rendered
{-# NOINLINE renderImage #-}

loadImage :: String -> Int -> Int -> IO V.Image
loadImage url width height =
  -- Catch fetch/render failures, but still let async exceptions terminate cleanly.
  go `catch` \e ->
    case fromException e :: Maybe SomeAsyncException of
      Just _  -> throwIO e
      Nothing -> pure (linesToImage width height ["Unable to render image: " ++ url])
  where
    go = do
      req <- parseRequest url
      resp <- httpLBS req
      tmpDir <- getTemporaryDirectory
      (tmpPath, handle) <- openTempFile tmpDir "agility-image"
      hClose handle
      let body = getResponseBody resp
      B.writeFile tmpPath body
      result <- renderImageBytes body tmpPath width height
      _ <- try (removeFile tmpPath) :: IO (Either IOException ())
      pure result

renderImageBytes :: B.ByteString -> FilePath -> Int -> Int -> IO V.Image
renderImageBytes body imagePath width height =
  case decodeImage (B.toStrict body) of
    Right dynImage -> pure (dynamicImageToBlockImage width height dynImage)
    Left _         -> renderImageFile imagePath width height

dynamicImageToBlockImage :: Int -> Int -> DynamicImage -> V.Image
dynamicImageToBlockImage cellWidth cellHeight dynImage =
  let scaled = scaleImageToPixels cellWidth (cellHeight * 2) (convertRGB8 dynImage)
   in VI.resize cellWidth cellHeight $
        VI.vertCat [blockRow scaled y | y <- [0, 2 .. imageHeight scaled - 1]]

scaleImageToPixels :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
scaleImageToPixels targetWidth targetHeight img =
  let boundedWidth = max 1 targetWidth
      boundedHeight = max 1 targetHeight
      srcWidth = imageWidth img
      srcHeight = imageHeight img
      scale = min (fromIntegral boundedWidth / fromIntegral srcWidth :: Double) (fromIntegral boundedHeight / fromIntegral srcHeight)
      scaledWidth = max 1 (min boundedWidth (round (fromIntegral srcWidth * scale)))
      scaledHeight = max 1 (min boundedHeight (round (fromIntegral srcHeight * scale)))
   in generateImage (sampleNearest img scaledWidth scaledHeight) scaledWidth scaledHeight

sampleNearest :: Image PixelRGB8 -> Int -> Int -> Int -> Int -> PixelRGB8
sampleNearest img targetWidth targetHeight x y =
  let srcX = min (imageWidth img - 1) (x * imageWidth img `div` targetWidth)
      srcY = min (imageHeight img - 1) (y * imageHeight img `div` targetHeight)
   in pixelAt img srcX srcY

blockRow :: Image PixelRGB8 -> Int -> V.Image
blockRow img y =
  VI.horizCat [blockCell (pixelAt img x y) (bottomPixel x) | x <- [0 .. imageWidth img - 1]]
  where
    bottomPixel x =
      if y + 1 < imageHeight img
        then Just (pixelAt img x (y + 1))
        else Nothing

blockCell :: PixelRGB8 -> Maybe PixelRGB8 -> V.Image
blockCell top Nothing =
  VI.char (V.withForeColor V.defAttr (pixelColor top)) '▀'
blockCell top (Just bottom) =
  VI.char (V.withBackColor (V.withForeColor V.defAttr (pixelColor top)) (pixelColor bottom)) '▀'

pixelColor :: PixelRGB8 -> V.Color
pixelColor (PixelRGB8 red green blue) =
  V.rgbColor (fromIntegral red :: Int) (fromIntegral green :: Int) (fromIntegral blue :: Int)

renderImageFile :: FilePath -> Int -> Int -> IO V.Image
renderImageFile imagePath width height = do
  chafaResult <- try (readProcessWithExitCode "chafa" ["--size", show width ++ "x" ++ show height, "--symbols", "block", "--colors", "full", "--color-space", "rgb", imagePath] "") :: IO (Either IOException (ExitCode, String, String))
  case chafaResult of
    Right (ExitSuccess, output, _) -> pure (ansiToImage width height output)
    _ -> do
      img2txtResult <- try (readProcessWithExitCode "img2txt" ["-W", show width, "-H", show height, "-f", "utf8", imagePath] "") :: IO (Either IOException (ExitCode, String, String))
      pure $
        case img2txtResult of
          Right (ExitSuccess, output, _) -> linesToImage width height (map stripAnsi (lines output))
          _                             -> linesToImage width height ["Unable to render image"]

data AnsiState = AnsiState
  { ansiFg :: Maybe V.Color,
    ansiBg :: Maybe V.Color
  }

ansiToImage :: Int -> Int -> String -> V.Image
ansiToImage width height output =
  VI.resize width height $
    VI.vertCat $
      take height (map lineToImage (parseAnsiLines output) ++ repeat blankLine)
  where
    blankLine = VI.charFill V.defAttr ' ' width 1
    lineToImage [] = blankLine
    lineToImage spans = VI.resizeWidth width (VI.horizCat (map spanToImage spans))
    spanToImage (attr, txt) = VI.string attr txt

parseAnsiLines :: String -> [[(V.Attr, String)]]
parseAnsiLines output =
  let parsed = reverse (go initialState [] [] [] output)
   in if null parsed then [[]] else parsed
  where
    initialState = AnsiState Nothing Nothing

    go st lineAcc txtAcc linesAcc [] =
      flushLine st lineAcc txtAcc : linesAcc
    go st lineAcc txtAcc linesAcc ('\n' : rest) =
      go st [] [] (flushLine st lineAcc txtAcc : linesAcc) rest
    go st lineAcc txtAcc linesAcc ('\ESC' : '[' : rest) =
      let (codes, afterCodes) = span (/= 'm') rest
          nextInput = drop 1 afterCodes
          nextLineAcc = flushText st lineAcc txtAcc
          nextState = applySgrCodes st (parseSgrCodes codes)
       in go nextState nextLineAcc [] linesAcc nextInput
    go st lineAcc txtAcc linesAcc (c : rest) =
      go st lineAcc (c : txtAcc) linesAcc rest

flushLine :: AnsiState -> [(V.Attr, String)] -> String -> [(V.Attr, String)]
flushLine st lineAcc txtAcc = reverse (flushText st lineAcc txtAcc)

flushText :: AnsiState -> [(V.Attr, String)] -> String -> [(V.Attr, String)]
flushText _ lineAcc [] = lineAcc
flushText st lineAcc txtAcc = (stateAttr st, reverse txtAcc) : lineAcc

parseSgrCodes :: String -> [Int]
parseSgrCodes "" = [0]
parseSgrCodes codes = map readCode (splitOnSemicolon codes)
  where
    readCode value =
      case reads value of
        [(n, "")] -> n
        _         -> 0

splitOnSemicolon :: String -> [String]
splitOnSemicolon [] = [""]
splitOnSemicolon value =
  let (part, rest) = break (== ';') value
   in case rest of
        []       -> [part]
        _ : more -> part : splitOnSemicolon more

applySgrCodes :: AnsiState -> [Int] -> AnsiState
applySgrCodes = go
  where
    go st [] = st
    go _ (0 : rest) = go (AnsiState Nothing Nothing) rest
    go st (39 : rest) = go st {ansiFg = Nothing} rest
    go st (49 : rest) = go st {ansiBg = Nothing} rest
    go st (38 : 2 : r : g : b : rest) = go st {ansiFg = Just (V.rgbColor r g b)} rest
    go st (48 : 2 : r : g : b : rest) = go st {ansiBg = Just (V.rgbColor r g b)} rest
    go st (38 : 5 : colorIdx : rest) = go st {ansiFg = ansi256Color colorIdx} rest
    go st (48 : 5 : colorIdx : rest) = go st {ansiBg = ansi256Color colorIdx} rest
    go st (code : rest)
      | code >= 30 && code <= 37 = go st {ansiFg = ansiBasicColor (code - 30)} rest
      | code >= 40 && code <= 47 = go st {ansiBg = ansiBasicColor (code - 40)} rest
      | code >= 90 && code <= 97 = go st {ansiFg = ansiBrightColor (code - 90)} rest
      | code >= 100 && code <= 107 = go st {ansiBg = ansiBrightColor (code - 100)} rest
    go st (_ : rest) = go st rest

ansiBasicColor :: Int -> Maybe V.Color
ansiBasicColor idx = case idx of
  0 -> Just V.black
  1 -> Just V.red
  2 -> Just V.green
  3 -> Just V.yellow
  4 -> Just V.blue
  5 -> Just V.magenta
  6 -> Just V.cyan
  7 -> Just V.white
  _ -> Nothing

ansiBrightColor :: Int -> Maybe V.Color
ansiBrightColor idx = case idx of
  0 -> Just (rgb 128 128 128)
  1 -> Just (rgb 255 0 0)
  2 -> Just (rgb 0 255 0)
  3 -> Just (rgb 255 255 0)
  4 -> Just (rgb 0 0 255)
  5 -> Just (rgb 255 0 255)
  6 -> Just (rgb 0 255 255)
  7 -> Just (rgb 255 255 255)
  _ -> Nothing

rgb :: Int -> Int -> Int -> V.Color
rgb = V.rgbColor

ansi256Color :: Int -> Maybe V.Color
ansi256Color idx
  | idx >= 0 && idx <= 15 = ansi16Color idx
  | idx >= 16 && idx <= 231 =
      let cubeIdx = idx - 16
          red = cubeComponent (cubeIdx `div` 36)
          green = cubeComponent ((cubeIdx `div` 6) `mod` 6)
          blue = cubeComponent (cubeIdx `mod` 6)
       in Just (rgb red green blue)
  | idx >= 232 && idx <= 255 =
      let level = 8 + (idx - 232) * 10
       in Just (rgb level level level)
  | otherwise = Nothing
  where
    cubeComponent value =
      if value == 0 then 0 else 55 + value * 40

ansi16Color :: Int -> Maybe V.Color
ansi16Color idx
  | idx < 8 = ansiBasicColor idx
  | otherwise = ansiBrightColor (idx - 8)

stateAttr :: AnsiState -> V.Attr
stateAttr st =
  let withFg = maybe V.defAttr (V.withForeColor V.defAttr) (ansiFg st)
   in maybe withFg (V.withBackColor withFg) (ansiBg st)

linesToImage :: Int -> Int -> [String] -> V.Image
linesToImage width height rows =
  VI.resize width height $
    VI.vertCat $
      map (VI.string V.defAttr) (fitLines width height rows)

fitLines :: Int -> Int -> [String] -> [String]
fitLines width height rows =
  take height (map fitLine rows ++ repeat (replicate width ' '))
  where
    fitLine line = take width line ++ replicate (max 0 (width - length line)) ' '

stripAnsi :: String -> String
stripAnsi [] = []
stripAnsi ('\ESC' : '[' : rest) = stripAnsi (dropAnsi rest)
stripAnsi (c : rest) = c : stripAnsi rest

dropAnsi :: String -> String
dropAnsi [] = []
dropAnsi (c : rest)
  | isDigit c || c == ';' = dropAnsi rest
  | otherwise = rest

rowLineCount :: [Int] -> Int -> Int -> Row -> Int
rowLineCount widths minH maxH row =
  let wrapped = zipWith (\width (txt, _) -> wrapOrTruncate width maxH txt) widths row
   in max minH (maximum (1 : map length wrapped))

paginateRows :: Int -> [(Int, Row, Int)] -> [[(Int, Row)]]
paginateRows limit = go [] 0 []
  where
    pageLimit = max 1 limit
    finish current pages =
      if null current then reverse pages else reverse (reverse current : pages)
    go current _ pages [] = finish current pages
    go [] _ pages ((rowIdx, row, height) : rest) =
      go [(rowIdx, row)] height pages rest
    go current currentHeight pages ((rowIdx, row, height) : rest)
      | currentHeight + height <= pageLimit =
          go ((rowIdx, row) : current) (currentHeight + height) pages rest
      | otherwise =
          go [(rowIdx, row)] height (reverse current : pages) rest

drawTable :: St -> Int -> TableConfig -> [Row] -> Widget Name
drawTable st idx cfg rows =
  case source cfg of
    ImageSource imageUrl refresh -> drawImageTable idx cfg imageUrl refresh
    _                            -> drawRowsTable st idx cfg rows

drawImageTable :: Int -> TableConfig -> String -> Int -> Widget Name
drawImageTable idx cfg imageUrl refreshSeconds = Widget Fixed Fixed $ do
  ctx <- getContext
  let avail = availWidth ctx
      height = availHeight ctx
      titleLines = maybe 0 (const 2) (title cfg)
      imageWidth = max 1 (avail - 2)
      availableImageHeight = max 1 (height - 2 - titleLines)
      imageHeight = min (maxColumnHeight cfg) availableImageHeight
      image = renderImage imageUrl refreshSeconds imageWidth imageHeight
      imageWidget = withAttr (textAttr idx) (raw image)
      titled widget =
        case title cfg of
          Just heading -> vBox [withAttr (tableTitleAttr idx) (str heading), padTop (Pad 1) widget]
          Nothing      -> widget
  render $
    withAttr (borderAttr idx) $
      border $
        titled imageWidget

drawRowsTable :: St -> Int -> TableConfig -> [Row] -> Widget Name
drawRowsTable st idx cfg rows = Widget Fixed Fixed $ do
  ctx <- getContext
  let avail = availWidth ctx
      height = availHeight ctx
      chromeWidth = length (columnWeights cfg) * 3
      colWs = distributeWidths (max 1 (avail - chromeWidth)) (columnWeights cfg)
      selCol = if activeTableIndex st == idx then fromMaybe (-1) (safeIndex (colPositions st) idx) else -1
      rowHeights = map (\(rowIdx, row) -> (rowIdx, row, rowLineCount colWs (minColumnHeight cfg) (maxColumnHeight cfg) row + 1)) (zip [0 ..] rows)
      headerHeight = maybe 0 (const 2) (columnHeaders cfg)
      reservedHeight titleLineCount = 2 + titleLineCount + headerHeight
      rowBudget titleLineCount = max 1 (height - reservedHeight titleLineCount)
      titleHeight = maybe 0 (const 2) (title cfg)
      firstPages = paginateRows (rowBudget titleHeight) rowHeights
      needsPageLabel = length firstPages > 1
      titleLines = if isJust (title cfg) || needsPageLabel then 2 else 0
      pages = paginateRows (rowBudget titleLines) rowHeights
      totalPages = max 1 (length pages)
      requestedPage = fromMaybe 0 (safeIndex (pagePositions st) idx)
      pageIdx = clampIndex 0 (totalPages - 1) requestedPage
      visibleRows = fromMaybe [] (safeIndex pages pageIdx)
      selectedPageRow =
        clampIndex
          0
          (max 0 (length visibleRows - 1))
          (fromMaybe 0 (safeIndex (rowPositions st) idx))
      selRow =
        if activeTableIndex st == idx
          then maybe (-1) fst (safeIndex visibleRows selectedPageRow)
          else -1
      pageLabel =
        if totalPages > 1
          then Just ("Page " ++ show (pageIdx + 1) ++ " of " ++ show totalPages)
          else Nothing
      headerWidgets = case columnHeaders cfg of
        Just headers -> [drawHeaderRow idx colWs headers, drawBorder idx colWs]
        Nothing      -> []
      tableLines = concatMap (drawRow idx colWs (minColumnHeight cfg) (maxColumnHeight cfg) selRow selCol) (zip [0 ..] visibleRows)
      allLines = headerWidgets ++ tableLines
      titleRow =
        foldr
          reportExtent
          ( hBox
              [ maybe emptyWidget (withAttr (tableTitleAttr idx) . str) (title cfg),
                padLeft Max (maybe emptyWidget (withAttr (tablePagingAttr idx) . str) pageLabel)
              ]
          )
          [PageName idx page | page <- [0 .. totalPages - 1]]
      titled widget =
        if titleLines > 0
          then vBox [titleRow, padTop (Pad 1) widget]
          else widget

  render $
    withAttr (borderAttr idx) $
      border $
        titled $
          vBox allLines

drawHeaderRow :: Int -> [Int] -> [String] -> Widget Name
drawHeaderRow idx colWs headers =
  hBox $
    zipWith
      ( \width heading ->
          withAttr (headerAttr idx) (str " " <+> padRight (Pad (width - length heading)) (str heading))
            <+> withAttr (borderAttr idx) (str " │")
      )
      colWs
      (take (length colWs) headers ++ repeat "")

drawRow :: Int -> [Int] -> Int -> Int -> Int -> Int -> (Int, (Int, Row)) -> [Widget Name]
drawRow tableIdx widths minH maxH selRow selCol (pageRowIdx, (rowIdx, row)) =
  let wrapped = zipWith (\width (txt, _) -> wrapOrTruncate width maxH txt) widths row
      rowHeight = max minH (maximum (1 : map length wrapped))
      padded = padCells rowHeight wrapped
      linesPerRow = transpose padded
      rowLines = map (drawLine tableIdx pageRowIdx rowIdx row widths selRow selCol) linesPerRow
      markRow []       = []
      markRow (w : ws) = reportExtent (RowName tableIdx pageRowIdx) w : ws
   in markRow rowLines ++ [drawBorder tableIdx widths]

drawLine :: Int -> Int -> Int -> Row -> [Int] -> Int -> Int -> [String] -> Widget Name
drawLine tableIdx pageRowIdx rowIdx row widths selRow selCol line =
  if all null line
    then drawSpacerLine widths
    else hBox $ zipWith4 (drawCell tableIdx pageRowIdx rowIdx row selRow selCol) [0 ..] line widths (repeat 1)

drawSpacerLine :: [Int] -> Widget Name
drawSpacerLine widths =
  str (replicate (sum (map (+ 3) widths)) ' ')

drawCell :: Int -> Int -> Int -> Row -> Int -> Int -> Int -> String -> Int -> Int -> Widget Name
drawCell tableIdx pageRowIdx rowIdx row selRow selCol colIdx txt width _ =
  let isSel = rowIdx == selRow && colIdx == selCol
      hasLink = maybe False (isJust . snd) (safeIndex row colIdx)
      attr
        | isSel = selectedTableAttr tableIdx
        | hasLink = linkAttr tableIdx
        | otherwise = textAttr tableIdx
      trailingPadding = replicate (max 0 (width - length txt)) ' '
      selectedRightPadding = if width > length txt then " " else ""
      selectedTailPadding = replicate (max 0 (width - length txt - 1)) ' '
      cell =
        if isSel
          then withAttr attr (str (" " ++ txt ++ selectedRightPadding)) <+> str selectedTailPadding
        else if hasLink
          then str " " <+> withAttr attr (str txt) <+> withAttr (textAttr tableIdx) (str trailingPadding)
          else withAttr attr (str (" " ++ txt ++ trailingPadding))
      bar = withAttr (borderAttr tableIdx) (str " │")
   in if null txt && not isSel
        then drawBlankCell width
        else reportExtent cellName (clickable cellName cell) <+> bar
  where
    cellName = CellName tableIdx pageRowIdx rowIdx colIdx

drawBlankCell :: Int -> Widget Name
drawBlankCell width =
  str (replicate (width + 3) ' ')

drawBorder :: Int -> [Int] -> Widget Name
drawBorder idx widths =
  let totalWidth = sum (map (+ 3) widths)
      line = replicate totalWidth '─'
   in withAttr (borderAttr idx) (str line)
