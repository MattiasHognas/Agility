module Agility.Dashboard
  ( distributeWidths,
    flattenLayoutItems,
    initialRowsForLayout,
    layoutItemCount,
  )
where

import Agility.Types
  ( LayoutItem (HorizontalGroup, TableItem),
    Row,
    TableConfig (source),
    TableSource (StaticSource),
  )

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
   in if sumW <= 0
        then
          let n = length weights
           in if n <= 0
                then []
                else
                  let baseWidth = max 1 (total `div` n)
                   in replicate n baseWidth
        else map (\weight -> max 1 $ weight * total `div` sumW) weights
