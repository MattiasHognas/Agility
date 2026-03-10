module Agility.UI.Draw
  ( app,
    drawUI,
  )
where

import Agility.Color (tableAttrs)
import Agility.Dashboard (distributeWidths, layoutItemCount)
import Agility.Interactive qualified as Interactive
import Agility.State (safeIndex)
import Agility.Types
  ( AppEvent,
    LayoutItem (HorizontalGroup, TableItem),
    Name,
    Row,
    St (dashboardItems, tableRowsData, tables),
  )
import Agility.UI.Table (drawTable)
import Brick
  ( App (..),
    Context (availWidth),
    Padding (Pad),
    Size (Fixed, Greedy),
    Widget (Widget, render),
    attrMap,
    emptyWidget,
    getContext,
    hBox,
    hLimit,
    neverShowCursor,
    padTop,
    str,
    vBox,
  )
import Brick.Widgets.Center (center)
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Graphics.Vty qualified as V

drawUI :: St -> [Widget Name]
drawUI st = [center $ layoutItemsWidget st (dashboardItems st) 0]

layoutItemsWidget :: St -> [LayoutItem] -> Int -> Widget Name
layoutItemsWidget _ [] _ = emptyWidget
layoutItemsWidget st [item] idx = drawLayoutItem st item idx
layoutItemsWidget st (item : items) idx =
  vBox
    [ drawLayoutItem st item idx,
      padTop (Pad 1) (layoutItemsWidget st items (idx + layoutItemCount item))
    ]

drawLayoutItem :: St -> LayoutItem -> Int -> Widget Name
drawLayoutItem st (TableItem cfg) idx =
  drawTable st idx cfg (tableRowsAt st idx)
drawLayoutItem st (HorizontalGroup weights cfgs) idx = Widget Greedy Fixed $ do
  ctx <- getContext
  let gapWidth = 2 * max 0 (length cfgs - 1)
      allocated = distributeWidths (max 1 (availWidth ctx - gapWidth)) weights
      tableWidgets =
        zipWith3
          (\width tableIdx cfg -> hLimit width (drawTable st tableIdx cfg (tableRowsAt st tableIdx)))
          allocated
          [idx ..]
          cfgs
  render $ hBox (intersperse (str "  ") tableWidgets)

tableRowsAt :: St -> Int -> [Row]
tableRowsAt st idx = fromMaybe [] (safeIndex (tableRowsData st) idx)

app :: App St AppEvent Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = Interactive.handleEvent,
      appStartEvent = pure (),
      appAttrMap = \st -> attrMap V.defAttr (tableAttrs (tables st))
    }
