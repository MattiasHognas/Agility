module Agility.Watcher
  ( refreshSourcesForLayout,
    startSourceThreads,
    watchConfig,
  )
where

import Agility.Config (decodeLayoutConfig)
import Agility.Dashboard (flattenLayoutItems)
import Agility.DataSource (fetchLocalRows, fetchWebRows, safeGetModificationTime)
import Agility.Types
  ( AppEvent (..),
    LayoutItem,
    TableConfig (source),
    TableSource (LocalSource, WebSource),
  )
import Brick.BChan (BChan, writeBChan)
import Control.Concurrent (MVar, ThreadId, forkIO, killThread, swapMVar, threadDelay)
import Control.Monad (forever, void)
import Data.ByteString.Lazy qualified as B
import System.Directory (getModificationTime)

refreshSourcesForLayout :: [LayoutItem] -> [(Int, TableSource)]
refreshSourcesForLayout items = zip [0 ..] (map source (flattenLayoutItems items))

startSourceThreads :: [(Int, TableSource)] -> BChan AppEvent -> IO [ThreadId]
startSourceThreads sources chan = fmap catMaybes (mapM forkSource sources)
  where
    catMaybes = foldr (maybe id (:)) []

    forkSource (idx, WebSource endpoint fm refresh) =
      Just
        <$> forkIO
          ( forever $ do
              rows <- fetchWebRows endpoint fm
              writeBChan chan (UpdateTable idx rows)
              threadDelay (refresh * 1000000)
          )
    forkSource (idx, LocalSource sourcePath fm refresh) =
      Just
        <$> forkIO
          ( do
              initialRows <- fetchLocalRows sourcePath fm
              writeBChan chan (UpdateTable idx initialRows)
              initialMod <- safeGetModificationTime sourcePath
              loop initialMod refresh
          )
      where
        loop lastMod secondsUntilRefresh = do
          threadDelay 1000000
          currentMod <- safeGetModificationTime sourcePath
          if currentMod /= lastMod
            then do
              rows <- fetchLocalRows sourcePath fm
              writeBChan chan (UpdateTable idx rows)
              loop currentMod refresh
            else
              if secondsUntilRefresh <= 1
                then do
                  rows <- fetchLocalRows sourcePath fm
                  writeBChan chan (UpdateTable idx rows)
                  loop currentMod refresh
                else loop currentMod (secondsUntilRefresh - 1)
    forkSource _ = pure Nothing

watchConfig :: FilePath -> BChan AppEvent -> MVar [ThreadId] -> IO ()
watchConfig configPath chan sourceThreadIds = do
  initialMod <- getModificationTime configPath
  loop initialMod
  where
    loop lastMod = do
      threadDelay 2000000
      newMod <- getModificationTime configPath
      nextMod <-
        if newMod > lastMod
          then do
            content <- B.readFile configPath
            case decodeLayoutConfig content of
              Right cfg -> do
                restartSourceThreads cfg
                writeBChan chan (ReloadConfig cfg)
                pure newMod
              Left err -> do
                putStrLn $ "JSON parse error: " ++ err
                pure lastMod
          else pure lastMod
      loop nextMod

    restartSourceThreads cfgs = do
      oldThreadIds <- swapMVar sourceThreadIds []
      mapM_ killThread oldThreadIds
      newThreadIds <- startSourceThreads (refreshSourcesForLayout cfgs) chan
      void (swapMVar sourceThreadIds newThreadIds)
