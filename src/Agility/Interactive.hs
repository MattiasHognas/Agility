module Agility.Interactive
  ( handleEvent,
    openUrl,
  )
where

import Agility.Dashboard (flattenLayoutItems, initialRowsForLayout)
import Agility.State
  ( cellUrlAt,
    cycleTable,
    moveSelection,
    normalizeSelection,
    selectedCellUrl,
    updateAt,
  )
import Agility.Types
  ( AppEvent (..),
    Name (..),
    St
      ( activeTableIndex,
        colPositions,
        configGeneration,
        dashboardItems,
        rowPositions,
        tableRowsData,
        tables
      ),
  )
import Brick
  ( BrickEvent (AppEvent, MouseDown, VtyEvent),
    EventM,
    gets,
    halt,
    modify,
  )
import Control.Exception (IOException, try)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Graphics.Vty qualified as V
import System.Info (os)
import System.Process (CreateProcess, createProcess, proc)

openUrl :: String -> IO Bool
openUrl target = tryCommands (openCommands target)

openCommands :: String -> [CreateProcess]
openCommands target = case os of
  "mingw32" ->
    [ proc "explorer.exe" [target],
      proc "cmd.exe" ["/c", "start", "", target]
    ]
  "darwin" -> [proc "open" [target]]
  _ -> [proc "xdg-open" [target]]

tryCommands :: [CreateProcess] -> IO Bool
tryCommands [] = pure False
tryCommands (command : rest) = do
  result <- try (void (createProcess command)) :: IO (Either IOException ())
  case result of
    Right () -> pure True
    Left _ -> tryCommands rest

handleEvent :: BrickEvent Name AppEvent -> EventM Name St ()
handleEvent (AppEvent (UpdateTable idx rows gen)) =
  modify $ \st ->
    if gen == configGeneration st
      then normalizeSelection st {tableRowsData = updateAt idx (const rows) (tableRowsData st)}
      else st
handleEvent (AppEvent (ReloadConfig cfgs)) =
  let flatTables = flattenLayoutItems cfgs
      rows = initialRowsForLayout cfgs
   in modify $ \st ->
        normalizeSelection
          st
            { dashboardItems = cfgs,
              tables = flatTables,
              tableRowsData = rows,
              rowPositions = replicate (length flatTables) 0,
              colPositions = replicate (length flatTables) 0,
              activeTableIndex = 0,
              configGeneration = configGeneration st + 1
            }
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = modify (moveSelection 0 (-1))
handleEvent (VtyEvent (V.EvKey V.KRight [])) = modify (moveSelection 0 1)
handleEvent (VtyEvent (V.EvKey V.KUp [])) = modify (moveSelection (-1) 0)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = modify (moveSelection 1 0)
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = modify (cycleTable 1)
handleEvent (VtyEvent (V.EvKey V.KBackTab [])) = modify (cycleTable (-1))
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- gets normalizeSelection
  case selectedCellUrl st of
    Just target -> void (liftIO (openUrl target))
    Nothing -> pure ()
handleEvent (MouseDown (CellName tableIdx rowIdx colIdx) V.BLeft _ _) = do
  modify $ \st ->
    normalizeSelection
      st
        { activeTableIndex = tableIdx,
          rowPositions = updateAt tableIdx (const rowIdx) (rowPositions (normalizeSelection st)),
          colPositions = updateAt tableIdx (const colIdx) (colPositions (normalizeSelection st))
        }
  st <- gets normalizeSelection
  case cellUrlAt st tableIdx rowIdx colIdx of
    Just target -> void (liftIO (openUrl target))
    Nothing -> pure ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent _ = pure ()
