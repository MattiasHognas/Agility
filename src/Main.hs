{-# LANGUAGE NumericUnderscores #-}

module Main where

import           Agility.Config             (decodeLayoutConfig)
import           Agility.Dashboard          (flattenLayoutItems,
                                             initialRowsForLayout)
import           Agility.State              (normalizeSelection)
import           Agility.Types              (St (..))
import           Agility.UI.Draw            (app)
import           Agility.Watcher            (refreshSourcesForLayout,
                                             startSourceThreads, watchConfig)
import           Brick                      (customMain)
import           Brick.BChan                (newBChan)
import           Control.Concurrent         (forkIO, newMVar)
import           Control.Exception          (IOException, try)
import           Control.Monad              (void)
import qualified Data.ByteString.Lazy       as B
import qualified Graphics.Vty               as V
import qualified Graphics.Vty.CrossPlatform as VCross

main :: IO ()
main = do
  let cfgFile = "tables.json"
  fileOrErr <- try (B.readFile cfgFile) :: IO (Either IOException B.ByteString)
  case fileOrErr of
    Left e ->
      putStrLn ("Failed to read config file " ++ cfgFile ++ ": " ++ show e)
    Right file ->
      case decodeLayoutConfig file of
        Left err -> putStrLn ("Failed to load config: " ++ err)
        Right layoutCfgs -> do
          chan <- newBChan 10
          let flatTables = flattenLayoutItems layoutCfgs
              refreshSources = refreshSourcesForLayout layoutCfgs
          initialSourceThreadIds <- startSourceThreads 0 refreshSources chan
          sourceThreadIds <- newMVar initialSourceThreadIds
          void $ forkIO $ watchConfig cfgFile chan sourceThreadIds
          let rows = initialRowsForLayout layoutCfgs
              st =
                St
                  0
                  (replicate (length flatTables) 0)
                  (replicate (length flatTables) 0)
                  layoutCfgs
                  flatTables
                  rows
                  0
              buildVty = do
                vty <- VCross.mkVty V.defaultConfig
                V.setMode (V.outputIface vty) V.Mouse True
                pure vty
          initialVty <- buildVty
          void $ customMain initialVty buildVty (Just chan) app (normalizeSelection st)
