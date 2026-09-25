module Agility.UI.Table
  ( drawTable,
  )
where

import           Agility.Color        (borderAttr, headerAttr, linkAttr,
                                       selectedTableAttr, tablePagingAttr,
                                       tableTitleAttr, textAttr)
import           Agility.Dashboard    (distributeWidths)
import           Agility.Media.Render (renderFrame)
import           Agility.State        (clampIndex, safeIndex)
import           Agility.Types        (MediaState (..), Name (..), Row,
                                       St (activeTableIndex, colPositions, pagePositions, rowPositions, tableMedia),
                                       TableConfig (columnHeaders, columnWeights, maxColumnHeight, minColumnHeight, source, title),
                                       TableSource (ImageSource))
import           Brick                (Context (availHeight, availWidth),
                                       Padding (Max, Pad), Size (Fixed),
                                       Widget (Widget, render), clickable,
                                       emptyWidget, getContext, hBox, padLeft,
                                       padRight, padTop, raw, reportExtent, str,
                                       vBox, withAttr, (<+>))
import           Brick.Widgets.Border (border)
import           Data.List            (transpose, zipWith4)
import           Data.Maybe           (fromMaybe, isJust)
import qualified Data.Text            as T
import qualified Graphics.Vty         as V

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
    ImageSource {} ->
      drawImageTable idx cfg (fromMaybe MediaLoading (safeIndex (tableMedia st) idx))
    _ -> drawRowsTable st idx cfg rows

drawImageTable :: Int -> TableConfig -> MediaState -> Widget Name
drawImageTable idx cfg media = Widget Fixed Fixed $ do
  ctx <- getContext
  let avail = availWidth ctx
      height = availHeight ctx
      titleLines = maybe 0 (const 2) (title cfg)
      imageWidth = max 1 (avail - 2)
      availableImageHeight = max 1 (height - 2 - titleLines)
      imageHeight = min (maxColumnHeight cfg) availableImageHeight
      message text = V.vertCat (map (V.string V.defAttr) (wrapOrTruncate imageWidth imageHeight text))
      image = case media of
        MediaReady frame -> renderFrame imageWidth imageHeight frame
        MediaLoading     -> message "Loading image..."
        MediaFailed err  -> message ("Unable to render image: " ++ err)
      imageWidget = withAttr (textAttr idx) (raw (V.resize imageWidth imageHeight image))
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
