module Agility.State
  ( cellUrlAt,
    clampIndex,
    colCount,
    cycleTable,
    movePage,
    normalizeSelection,
    moveSelection,
    rowCount,
    safeIndex,
    tableCount,
    updateAt,
  )
where

import           Agility.Types (St (activeTableIndex, colPositions, pagePositions, rowPositions, tableRowsData, tables))
import           Data.Maybe    (fromMaybe)

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs idx
  | idx < 0 = Nothing
  | otherwise = go idx xs
  where
    go 0 (y : _)  = Just y
    go n (_ : ys) = go (n - 1) ys
    go _ []       = Nothing

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt idx f xs
  | idx < 0 || idx >= length xs = xs
  | otherwise =
      case splitAt idx xs of
        (before, x : after) -> before ++ f x : after
        _                   -> xs

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
  | tableCount st == 0 = st {activeTableIndex = 0, rowPositions = [], colPositions = [], pagePositions = []}
  | otherwise =
      let tableTotal = tableCount st
          activeIdx = clampIndex 0 (tableTotal - 1) (activeTableIndex st)
          fixedRows = take tableTotal (rowPositions st ++ repeat 0)
          fixedCols = take tableTotal (colPositions st ++ repeat 0)
          fixedPages = take tableTotal (pagePositions st ++ repeat 0)
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
              colPositions = map snd normalized,
              pagePositions = map (max 0) fixedPages
            }

moveSelection :: Int -> Int -> St -> St
moveSelection dRow dCol st =
  let normalized = normalizeSelection st
      tableIdx = activeTableIndex normalized
      rows = rowPositions normalized
      cols = colPositions normalized
      currentRow = fromMaybe 0 (safeIndex rows tableIdx)
      nextRowMax = rowCount normalized tableIdx - 1
      nextRow = if nextRowMax < 0 then 0 else clampIndex 0 nextRowMax (currentRow + dRow)
      nextColMax = colCount normalized tableIdx nextRow - 1
      currentCol = fromMaybe 0 (safeIndex cols tableIdx)
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

movePage :: Int -> St -> St
movePage delta st =
  let normalized = normalizeSelection st
      tableIdx = activeTableIndex normalized
      pages = pagePositions normalized
      rows = rowPositions normalized
      currentPage = fromMaybe 0 (safeIndex pages tableIdx)
      nextPage = max 0 (currentPage + delta)
   in normalized
        { pagePositions = updateAt tableIdx (const nextPage) pages,
          rowPositions = updateAt tableIdx (const 0) rows
        }

cellUrlAt :: St -> Int -> Int -> Int -> Maybe String
cellUrlAt st tableIdx rowIdx colIdx = do
  rows <- safeIndex (tableRowsData st) tableIdx
  row <- safeIndex rows rowIdx
  (_, mUrl) <- safeIndex row colIdx
  mUrl
