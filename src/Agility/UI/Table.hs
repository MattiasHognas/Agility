module Agility.UI.Table
  ( drawTable,
  )
where

import Agility.Color
  ( borderAttr,
    headerAttr,
    linkAttr,
    selectedTableAttr,
    tableTitleAttr,
    textAttr,
  )
import Agility.Dashboard (distributeWidths)
import Agility.State (safeIndex)
import Agility.Types
  ( Name (..),
    Row,
    St (activeTableIndex, colPositions, rowPositions),
    TableConfig
      ( columnHeaders,
        columnWeights,
        maxColumnHeight,
        minColumnHeight,
        title
      ),
  )
import Brick
  ( Context (availWidth),
    Padding (Pad),
    Size (Fixed),
    Widget (Widget, render),
    clickable,
    getContext,
    hBox,
    padRight,
    padTop,
    str,
    vBox,
    withAttr,
    (<+>),
  )
import Brick.Widgets.Border (border)
import Data.List (transpose, zipWith4)
import Data.Text qualified as T

wrapOrTruncate :: Int -> Int -> String -> [String]
wrapOrTruncate width maxHeight txt =
  let chunks = map T.unpack $ T.chunksOf width (T.pack txt)
   in if length chunks <= maxHeight
        then chunks
        else take (maxHeight - 1) chunks ++ [take (width - 3) (chunks !! (maxHeight - 1)) ++ "..."]

padCells :: Int -> [[String]] -> [[String]]
padCells height = map (\xs -> xs ++ replicate (height - length xs) "")

drawTable :: St -> Int -> TableConfig -> [Row] -> Widget Name
drawTable st idx cfg rows = Widget Fixed Fixed $ do
  ctx <- getContext
  let avail = availWidth ctx
      chromeWidth = length (columnWeights cfg) * 3
      colWs = distributeWidths (max 1 (avail - chromeWidth)) (columnWeights cfg)
      selRow = if activeTableIndex st == idx then rowPositions st !! idx else -1
      selCol = if activeTableIndex st == idx then colPositions st !! idx else -1
      headerWidgets = case columnHeaders cfg of
        Just headers -> [drawHeaderRow idx colWs headers, drawBorder idx colWs]
        Nothing -> []
      tableLines = concatMap (drawRow idx colWs (minColumnHeight cfg) (maxColumnHeight cfg) selRow selCol) (zip [0 ..] rows)
      allLines = headerWidgets ++ tableLines
      titled widget = case title cfg of
        Just heading -> vBox [withAttr (tableTitleAttr idx) (str heading), padTop (Pad 1) widget]
        Nothing -> widget

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
            <+> withAttr (borderAttr idx) (str " |")
      )
      colWs
      (take (length colWs) headers ++ repeat "")

drawRow :: Int -> [Int] -> Int -> Int -> Int -> Int -> (Int, Row) -> [Widget Name]
drawRow tableIdx widths minH maxH selRow selCol (rowIdx, row) =
  let wrapped = zipWith (\width (txt, _) -> wrapOrTruncate width maxH txt) widths row
      rowHeight = max minH (maximum (1 : map length wrapped))
      padded = padCells rowHeight wrapped
      linesPerRow = transpose padded
   in map (drawLine tableIdx rowIdx row widths selRow selCol) linesPerRow ++ [drawBorder tableIdx widths]

drawLine :: Int -> Int -> Row -> [Int] -> Int -> Int -> [String] -> Widget Name
drawLine tableIdx rowIdx row widths selRow selCol line =
  if all null line
    then drawSpacerLine widths
    else hBox $ zipWith4 (drawCell tableIdx rowIdx row selRow selCol) [0 ..] line widths (repeat 1)

drawSpacerLine :: [Int] -> Widget Name
drawSpacerLine widths =
  str (replicate (sum (map (+ 3) widths)) ' ')

drawCell :: Int -> Int -> Row -> Int -> Int -> Int -> String -> Int -> Int -> Widget Name
drawCell tableIdx rowIdx row selRow selCol colIdx txt width _ =
  let isSel = rowIdx == selRow && colIdx == selCol
      hasLink = maybe False ((/= Nothing) . snd) (safeIndex row colIdx)
      attr
        | isSel = selectedTableAttr tableIdx
        | hasLink = linkAttr tableIdx
        | otherwise = textAttr tableIdx
      cell = withAttr attr (str " " <+> padRight (Pad (width - length txt)) (str txt))
      bar = withAttr (borderAttr tableIdx) (str " |")
   in if null txt && not isSel
        then drawBlankCell width
        else clickable (CellName tableIdx rowIdx colIdx) cell <+> bar

drawBlankCell :: Int -> Widget Name
drawBlankCell width =
  str (replicate (width + 3) ' ')

drawBorder :: Int -> [Int] -> Widget Name
drawBorder idx widths =
  withAttr (borderAttr idx) $
    str "+" <+> hBox (map (\width -> str (replicate (width + 3) '-')) widths) <+> str "+"
