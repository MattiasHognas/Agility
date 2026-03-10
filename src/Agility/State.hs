module Agility.State
  ( cellUrlAt,
    clampIndex,
    colCount,
    cycleTable,
    normalizeSelection,
    moveSelection,
    rowCount,
    safeIndex,
    selectedCellUrl,
    tableCount,
    updateAt,
  )
where

import Agility.Types

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs idx
  | idx < 0 || idx >= length xs = Nothing
  | otherwise = Just (xs !! idx)

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs = take idx xs ++ [f (xs !! idx)] ++ drop (idx + 1) xs

clampIndex :: Int -> Int -> Int -> Int
clampIndex lo hi value = max lo (min hi value)

tableCount :: St -> Int
tableCount = length . tables

rowCount :: St -> Int -> Int
rowCount st tableIdx = maybe 0 length (safeIndex (tableRowsData st) tableIdx)

colCount :: St -> Int -> Int -> Int
colCount st tableIdx rowIdx = maybe 0 length $ do
  rows <- safeIndex (tableRowsData st) tableIdx
  safeIndex rows rowIdx

normalizeSelection :: St -> St
normalizeSelection st
  | tableCount st == 0 = st {activeTableIndex = 0, rowPositions = [], colPositions = []}
  | otherwise =
      let tableTotal = tableCount st
          activeIdx = clampIndex 0 (tableTotal - 1) (activeTableIndex st)
          fixedRows = take tableTotal (rowPositions st ++ repeat 0)
          fixedCols = take tableTotal (colPositions st ++ repeat 0)
          normalizeAt idx (rowPos, colPos) =
            let rows = rowCount st idx
                newRow = if rows == 0 then 0 else clampIndex 0 (rows - 1) rowPos
                cols = colCount st idx newRow
                newCol = if cols == 0 then 0 else clampIndex 0 (cols - 1) colPos
             in (newRow, newCol)
          normalized = zipWith normalizeAt [0 ..] (zip fixedRows fixedCols)
       in st
            { activeTableIndex = activeIdx,
              rowPositions = map fst normalized,
              colPositions = map snd normalized
            }

moveSelection :: Int -> Int -> St -> St
moveSelection dRow dCol st =
  let normalized = normalizeSelection st
      tableIdx = activeTableIndex normalized
      rows = rowPositions normalized
      cols = colPositions normalized
      currentRow = rows !! tableIdx
      nextRowMax = rowCount normalized tableIdx - 1
      nextRow = if nextRowMax < 0 then 0 else clampIndex 0 nextRowMax (currentRow + dRow)
      nextColMax = colCount normalized tableIdx nextRow - 1
      currentCol = cols !! tableIdx
      nextCol = if nextColMax < 0 then 0 else clampIndex 0 nextColMax (currentCol + dCol)
   in normalized
        { rowPositions = updateAt tableIdx (const nextRow) rows,
          colPositions = updateAt tableIdx (const nextCol) cols
        }

cycleTable :: Int -> St -> St
cycleTable delta st
  | tableCount st == 0 = st
  | otherwise =
      let normalized = normalizeSelection st
          total = tableCount normalized
          nextIdx = (activeTableIndex normalized + delta + total) `mod` total
       in normalizeSelection normalized {activeTableIndex = nextIdx}

cellUrlAt :: St -> Int -> Int -> Int -> Maybe String
cellUrlAt st tableIdx rowIdx colIdx = do
  rows <- safeIndex (tableRowsData st) tableIdx
  row <- safeIndex rows rowIdx
  (_, mUrl) <- safeIndex row colIdx
  mUrl

selectedCellUrl :: St -> Maybe String
selectedCellUrl st = do
  normalized <- Just (normalizeSelection st)
  let tableIdx = activeTableIndex normalized
  rowIdx <- safeIndex (rowPositions normalized) tableIdx
  colIdx <- safeIndex (colPositions normalized) tableIdx
  cellUrlAt normalized tableIdx rowIdx colIdx
