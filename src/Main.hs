{-# LANGUAGE NumericUnderscores #-}

module Main where

import Agility.Config (decodeLayoutConfig)
import Agility.Dashboard (flattenLayoutItems, initialRowsForLayout)
import Agility.State (normalizeSelection)
import Agility.Types (St (..))
import Agility.UI.Draw (app)
import Agility.Watcher (refreshSourcesForLayout, startSourceThreads, watchConfig)
import Brick (customMain)
import Brick.BChan (newBChan)
import Control.Concurrent (forkIO, newMVar)
import Control.Monad (void)
import Data.ByteString.Lazy qualified as B
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform qualified as VCross

main :: IO ()
main = do
  let cfgFile = "tables.json"
  file <- B.readFile cfgFile
  case decodeLayoutConfig file of
    Left err -> putStrLn ("Failed to load config: " ++ err)
    Right layoutCfgs -> do
      chan <- newBChan 10
      let flatTables = flattenLayoutItems layoutCfgs
          refreshSources = refreshSourcesForLayout layoutCfgs
      initialSourceThreadIds <- startSourceThreads refreshSources chan
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
          buildVty = do
            vty <- VCross.mkVty V.defaultConfig
            V.setMode (V.outputIface vty) V.Mouse True
            pure vty
      initialVty <- buildVty
      void $ customMain initialVty buildVty (Just chan) app (normalizeSelection st)
