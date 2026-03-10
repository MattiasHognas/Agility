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
import Control.Exception (IOException, try)
import Control.Monad (forever, void)
import Data.ByteString.Lazy qualified as B

refreshSourcesForLayout :: [LayoutItem] -> [(Int, TableSource)]
refreshSourcesForLayout items = zip [0 ..] (map source (flattenLayoutItems items))

startSourceThreads :: Int -> [(Int, TableSource)] -> BChan AppEvent -> IO [ThreadId]
startSourceThreads gen sources chan = fmap catMaybes (mapM forkSource sources)
  where
    catMaybes = foldr (maybe id (:)) []

    forkSource (idx, WebSource endpoint fm refresh) =
      Just
        <$> forkIO
          ( forever $ do
              rows <- fetchWebRows endpoint fm
              writeBChan chan (UpdateTable idx rows gen)
              threadDelay (refresh * 1000000)
          )
    forkSource (idx, LocalSource sourcePath fm refresh) =
      Just
        <$> forkIO
          ( do
              initialRows <- fetchLocalRows sourcePath fm
              writeBChan chan (UpdateTable idx initialRows gen)
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
              writeBChan chan (UpdateTable idx rows gen)
              loop currentMod refresh
            else
              if secondsUntilRefresh <= 1
                then do
                  rows <- fetchLocalRows sourcePath fm
                  writeBChan chan (UpdateTable idx rows gen)
                  loop currentMod refresh
                else loop currentMod (secondsUntilRefresh - 1)
    forkSource _ = pure Nothing

watchConfig :: FilePath -> BChan AppEvent -> MVar [ThreadId] -> IO ()
watchConfig configPath chan sourceThreadIds = do
  initialMod <- safeGetModificationTime configPath
  loop initialMod 0
  where
    loop lastMod currentGen = do
      threadDelay 2000000
      newMod <- safeGetModificationTime configPath
      (nextMod, nextGen) <-
        if newMod > lastMod
          then do
            result <- try (B.readFile configPath) :: IO (Either IOException B.ByteString)
            case result of
              Left err -> do
                putStrLn $ "Config file read error: " ++ show err
                pure (lastMod, currentGen)
              Right content ->
                case decodeLayoutConfig content of
                  Right cfg -> do
                    let newGen = currentGen + 1
                    restartSourceThreads cfg newGen
                    writeBChan chan (ReloadConfig cfg)
                    pure (newMod, newGen)
                  Left err -> do
                    putStrLn $ "JSON parse error: " ++ err
                    pure (lastMod, currentGen)
          else pure (lastMod, currentGen)
      loop nextMod nextGen

    restartSourceThreads cfgs gen = do
      oldThreadIds <- swapMVar sourceThreadIds []
      mapM_ killThread oldThreadIds
      newThreadIds <- startSourceThreads gen (refreshSourcesForLayout cfgs) chan
      void (swapMVar sourceThreadIds newThreadIds)
