module Agility.Dashboard
  ( distributeWidths,
    flattenLayoutItems,
    initialRowsForLayout,
    layoutItemCount,
  )
where

import Agility.Types

layoutItemCount :: LayoutItem -> Int
layoutItemCount (TableItem _) = 1
layoutItemCount (HorizontalGroup _ cfgs) = length cfgs

flattenLayoutItems :: [LayoutItem] -> [TableConfig]
flattenLayoutItems = concatMap flattenOne
  where
    flattenOne (TableItem cfg) = [cfg]
    flattenOne (HorizontalGroup _ cfgs) = cfgs

initialRowsForLayout :: [LayoutItem] -> [[Row]]
initialRowsForLayout = map rowsForTable . flattenLayoutItems
  where
    rowsForTable cfg = case source cfg of
      StaticSource rs -> rs
      _ -> []

distributeWidths :: Int -> [Int] -> [Int]
distributeWidths total weights =
  let sumW = sum weights
   in map (\weight -> max 1 $ weight * total `div` sumW) weights
