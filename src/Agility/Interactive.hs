module Agility.Interactive
  ( handleEvent,
    openUrl,
  )
where

import           Agility.Dashboard      (flattenLayoutItems,
                                         initialRowsForLayout)
import           Agility.State          (cellUrlAt, cycleTable, movePage, moveSelection,
                                         normalizeSelection, rowCount, safeIndex,
                                         updateAt)
import           Agility.Types          (AppEvent (..), Name (..),
                                         St (activeTableIndex, colPositions, configGeneration, dashboardItems, pagePositions, rowPositions, tableRowsData, tables))
import           Brick                  (BrickEvent (AppEvent, MouseDown, VtyEvent),
                                         EventM, gets, halt, lookupExtent, modify)
import           Control.Exception      (IOException, try)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (fromMaybe)
import qualified Graphics.Vty           as V
import           System.Info            (os)
import           System.Process         (CreateProcess, createProcess, proc)

openUrl :: String -> IO Bool
openUrl target = tryCommands (openCommands target)

openCommands :: String -> [CreateProcess]
openCommands target = case os of
  "linux" -> [proc "xdg-open" [target]]
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
    Left _   -> tryCommands rest

moveVisibleSelection :: Int -> EventM Name St ()
moveVisibleSelection delta = do
  st <- gets normalizeSelection
  let tableIdx = activeTableIndex st
      currentRow = fromMaybe 0 (safeIndex (rowPositions st) tableIdx)
      candidateRow = currentRow + delta
  if candidateRow < 0
    then pure ()
    else do
      mExtent <- lookupExtent (RowName tableIdx candidateRow)
      case mExtent of
        Just _  -> modify (moveSelection delta 0)
        Nothing -> pure ()

moveVisiblePage :: Int -> EventM Name St ()
moveVisiblePage delta = do
  st <- gets normalizeSelection
  let tableIdx = activeTableIndex st
      currentPage = fromMaybe 0 (safeIndex (pagePositions st) tableIdx)
      candidatePage = currentPage + delta
  if candidatePage < 0
    then pure ()
    else do
      mExtent <- lookupExtent (PageName tableIdx candidatePage)
      case mExtent of
        Just _  -> modify (movePage delta)
        Nothing -> pure ()

selectedVisibleCellUrl :: St -> EventM Name St (Maybe String)
selectedVisibleCellUrl st = do
  let normalized = normalizeSelection st
      tableIdx = activeTableIndex normalized
      pageRowIdx = fromMaybe 0 (safeIndex (rowPositions normalized) tableIdx)
      colIdx = fromMaybe 0 (safeIndex (colPositions normalized) tableIdx)
      candidates = [0 .. rowCount normalized tableIdx - 1]
  findVisibleCell normalized tableIdx pageRowIdx colIdx candidates

findVisibleCell :: St -> Int -> Int -> Int -> [Int] -> EventM Name St (Maybe String)
findVisibleCell _ _ _ _ [] = pure Nothing
findVisibleCell st tableIdx pageRowIdx colIdx (rowIdx : rest) = do
  mExtent <- lookupExtent (CellName tableIdx pageRowIdx rowIdx colIdx)
  case mExtent of
    Just _  -> pure (cellUrlAt st tableIdx rowIdx colIdx)
    Nothing -> findVisibleCell st tableIdx pageRowIdx colIdx rest

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
              pagePositions = replicate (length flatTables) 0,
              activeTableIndex = 0,
              configGeneration = configGeneration st + 1
            }
handleEvent (VtyEvent (V.EvKey V.KLeft [])) = modify (moveSelection 0 (-1))
handleEvent (VtyEvent (V.EvKey V.KRight [])) = modify (moveSelection 0 1)
handleEvent (VtyEvent (V.EvKey V.KUp [])) = moveVisibleSelection (-1)
handleEvent (VtyEvent (V.EvKey V.KDown [])) = moveVisibleSelection 1
handleEvent (VtyEvent (V.EvKey V.KPageUp [])) = moveVisiblePage (-1)
handleEvent (VtyEvent (V.EvKey V.KPageDown [])) = moveVisiblePage 1
handleEvent (VtyEvent (V.EvKey (V.KChar '-') [])) = moveVisiblePage (-1)
handleEvent (VtyEvent (V.EvKey (V.KChar '+') [])) = moveVisiblePage 1
handleEvent (VtyEvent (V.EvKey (V.KChar '\t') [])) = modify (cycleTable 1)
handleEvent (VtyEvent (V.EvKey V.KBackTab [])) = modify (cycleTable (-1))
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  st <- gets normalizeSelection
  mTarget <- selectedVisibleCellUrl st
  case mTarget of
    Just target -> void (liftIO (openUrl target))
    Nothing     -> pure ()
handleEvent (MouseDown (CellName tableIdx pageRowIdx rowIdx colIdx) V.BLeft _ _) = do
  modify $ \st ->
    let normalized = normalizeSelection st
     in normalizeSelection
          normalized
            { activeTableIndex = tableIdx,
              rowPositions = updateAt tableIdx (const pageRowIdx) (rowPositions normalized),
              colPositions = updateAt tableIdx (const colIdx) (colPositions normalized)
            }
  st <- gets normalizeSelection
  case cellUrlAt st tableIdx rowIdx colIdx of
    Just target -> void (liftIO (openUrl target))
    Nothing     -> pure ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent _ = pure ()
