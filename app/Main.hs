{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

import Brick hiding (Horizontal, Vertical, txt)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border hiding (borderAttr)
import Brick.Widgets.Center
import Control.Applicative ((<|>))
import Control.Concurrent
import Control.Exception (IOException, try)
import Control.Monad (forever, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as B
import Data.Char (toLower)
import Data.List (intersperse, transpose, zipWith4)
import Data.Maybe (catMaybes, mapMaybe)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import GHC.Generics
import Graphics.Vty qualified as V
import Graphics.Vty.CrossPlatform qualified as VCross
import Network.HTTP.Simple
import Numeric (readHex)
import System.Directory (getModificationTime)
import System.Info (os)
import System.Process (createProcess, proc)

data Name = CellName Int Int Int deriving (Eq, Ord, Show)

type Cell = (String, Maybe String)

type Row = [Cell]

data TableSource
  = StaticSource [[Cell]]
  | WebSource
      { url :: String,
        fields :: FieldMapping,
        refreshSeconds :: Int
      }
  | LocalSource
      { path :: FilePath,
        fields :: FieldMapping,
        refreshSeconds :: Int
      }
  deriving (Show)

data FieldMapping = FieldMapping
  { fieldNames :: [String]
  }
  deriving (Show, Generic)

data ColorConfig = ColorConfig
  { textColor :: Maybe String,
    borderColor :: Maybe String,
    titleColor :: Maybe String,
    headerColor :: Maybe String,
    selectedTextColor :: Maybe String,
    selectedBgColor :: Maybe String
  }
  deriving (Show)

data TableConfig = TableConfig
  { title :: Maybe String,
    columnHeaders :: Maybe [String],
    columnWeights :: [Int],
    minColumnHeight :: Int,
    maxColumnHeight :: Int,
    colors :: Maybe ColorConfig,
    source :: TableSource
  }
  deriving (Show)

data LayoutItem
  = TableItem TableConfig
  | HorizontalGroup
      { tableWeights :: [Int],
        groupedTables :: [TableConfig]
      }
  deriving (Show)

data St = St
  { activeTableIndex :: Int,
    rowPositions :: [Int],
    colPositions :: [Int],
    dashboardItems :: [LayoutItem],
    tables :: [TableConfig],
    tableRowsData :: [[Row]]
  }
  deriving (Show)

data AppEvent
  = UpdateTable Int [Row]
  | ReloadConfig [LayoutItem]
  deriving (Show)

textAttr :: Int -> AttrName
textAttr i = attrName ("table" ++ show i ++ ".text")

borderAttr :: Int -> AttrName
borderAttr i = attrName ("table" ++ show i ++ ".border")

headerAttr :: Int -> AttrName
headerAttr i = attrName ("table" ++ show i ++ ".header")

linkAttr :: Int -> AttrName
linkAttr i = attrName ("table" ++ show i ++ ".link")

tableTitleAttr :: Int -> AttrName
tableTitleAttr i = attrName ("table" ++ show i ++ ".title")

selectedTableAttr :: Int -> AttrName
selectedTableAttr i = attrName ("table" ++ show i ++ ".selected")

instance FromJSON FieldMapping where
  parseJSON v =
    (FieldMapping <$> parseJSON v)
      <|> withObject
        "FieldMapping"
        ( \obj ->
            FieldMapping
              <$> sequence
                [ obj .: "title",
                  obj .: "body",
                  obj .: "id"
                ]
        )
        v

parseValidColor :: Object -> String -> Parser (Maybe String)
parseValidColor v keyStr = do
  ms <- v .:? K.fromString keyStr
  case ms of
    Nothing -> pure Nothing
    Just s -> case parseColor s of
      Just _ -> pure (Just s)
      Nothing ->
        fail $
          "Invalid color value for '"
            ++ keyStr
            ++ "': \""
            ++ s
            ++ "\". Use a named color (e.g. \"red\", \"blue\") or a hex value (e.g. \"#ff0000\")."

instance FromJSON ColorConfig where
  parseJSON = withObject "ColorConfig" $ \v ->
    ColorConfig
      <$> parseValidColor v "text"
      <*> parseValidColor v "border"
      <*> parseValidColor v "title"
      <*> parseValidColor v "header"
      <*> parseValidColor v "selectedText"
      <*> parseValidColor v "selectedBg"

instance FromJSON TableSource where
  parseJSON = withObject "TableSource" $ \v -> do
    typ <- v .: "type"
    case (typ :: String) of
      "static" -> do
        rawRows <- v .: "rows"
        let parseCell [txt, mUrl] = (txt, Just mUrl)
            parseCell [txt] = (txt, Nothing)
            parseCell _ = ("", Nothing)
        pure $ StaticSource (map (map parseCell) rawRows)
      "web" ->
        WebSource
          <$> v .: "url"
          <*> v .: "fields"
          <*> v .:? "refreshSeconds" .!= 5
      "local" ->
        LocalSource
          <$> v .: "path"
          <*> v .: "fields"
          <*> v .:? "refreshSeconds" .!= 5
      _ -> fail "Unknown source type"

instance FromJSON TableConfig where
  parseJSON = withObject "TableConfig" $ \v -> do
    minH <- v .:? "minColumnHeight" .!= 1
    maxH <- v .:? "maxColumnHeight" .!= minH
    if minH < 1
      then fail "minColumnHeight must be at least 1"
      else
        if maxH < minH
          then fail "maxColumnHeight must be greater than or equal to minColumnHeight"
          else
            TableConfig
              <$> v .:? "title"
              <*> v .:? "columnHeaders"
              <*> v .: "columnWeights"
              <*> pure minH
              <*> pure maxH
              <*> v .:? "colors"
              <*> v .: "source"

instance FromJSON LayoutItem where
  parseJSON = withObject "LayoutItem" $ \obj ->
    if KM.member (K.fromString "tableWeights") obj || KM.member (K.fromString "tables") obj
      then parseGroup obj
      else TableItem <$> parseJSON (Object obj)
    where
      parseGroup obj = do
        weights <- obj .: "tableWeights"
        cfgs <- obj .: "tables"
        when (null cfgs) $ fail "horizontal group must include at least one table"
        when (length weights /= length cfgs) $ fail "tableWeights must match the number of tables"
        pure (HorizontalGroup weights cfgs)

parseNamedColor :: String -> Maybe V.Color
parseNamedColor s =
  case map toLower s of
    "black" -> Just V.black
    "red" -> Just V.red
    "green" -> Just V.green
    "yellow" -> Just V.yellow
    "blue" -> Just V.blue
    "magenta" -> Just V.magenta
    "cyan" -> Just V.cyan
    "white" -> Just V.white
    _ -> Nothing

parseHexColor :: String -> Maybe V.Color
parseHexColor ('#' : xs)
  | length xs == 6 =
      case mapM parseHexByte [take 2 xs, take 2 (drop 2 xs), take 2 (drop 4 xs)] of
        Just [r, g, b] -> Just (V.rgbColor r g b)
        _ -> Nothing
  where
    parseHexByte :: String -> Maybe Int
    parseHexByte h = case readHex h of
      [(n, "")] -> Just n
      _ -> Nothing
parseHexColor _ = Nothing

parseColor :: String -> Maybe V.Color
parseColor s = parseNamedColor s <|> parseHexColor s

mkAttr :: Maybe String -> Maybe String -> V.Attr
mkAttr mFg mBg =
  let base = V.defAttr
      withF = maybe base (\s -> maybe base (\c -> V.withForeColor base c) (parseColor s)) mFg
   in maybe withF (\s -> maybe withF (\c -> V.withBackColor withF c) (parseColor s)) mBg

tableAttrs :: [TableConfig] -> [(AttrName, V.Attr)]
tableAttrs cfgs =
  concatMap one (zip [0 ..] cfgs)
  where
    one (i, cfg) =
      let c = colors cfg
          txt = c >>= textColor
          bord = c >>= borderColor
          ttl = c >>= titleColor
          hdr = c >>= headerColor
          selFg = c >>= selectedTextColor
          selBg = c >>= selectedBgColor
       in [ (textAttr i, mkAttr txt Nothing),
            (borderAttr i, mkAttr bord Nothing),
            (headerAttr i, mkAttr hdr Nothing),
            (linkAttr i, V.withStyle (mkAttr (txt <|> hdr <|> Just "cyan") Nothing) V.underline),
            (tableTitleAttr i, V.withStyle (mkAttr ttl Nothing) V.bold),
            ( selectedTableAttr i,
              case selFg <|> selBg of
                Nothing -> fg V.yellow
                Just _ -> mkAttr selFg selBg
            )
          ]

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
          (\w tableIdx cfg -> hLimit w (drawTable st tableIdx cfg (tableRowsAt st tableIdx)))
          allocated
          [idx ..]
          cfgs
  render $ hBox (intersperse (str "  ") tableWidgets)

layoutItemCount :: LayoutItem -> Int
layoutItemCount (TableItem _) = 1
layoutItemCount (HorizontalGroup _ cfgs) = length cfgs

flattenLayoutItems :: [LayoutItem] -> [TableConfig]
flattenLayoutItems = concatMap flattenOne
  where
    flattenOne (TableItem cfg) = [cfg]
    flattenOne (HorizontalGroup _ cfgs) = cfgs

tableRowsAt :: St -> Int -> [Row]
tableRowsAt st idx = maybe [] id (safeIndex (tableRowsData st) idx)

initialRowsForLayout :: [LayoutItem] -> [[Row]]
initialRowsForLayout = map rowsForTable . flattenLayoutItems
  where
    rowsForTable cfg = case source cfg of
      StaticSource rs -> rs
      _ -> []

drawTable :: St -> Int -> TableConfig -> [Row] -> Widget Name
drawTable st idx cfg rows = Widget Fixed Fixed $ do
  ctx <- getContext
  let avail = availWidth ctx
      chromeWidth = length (columnWeights cfg) * 3
      colWs = distributeWidths (max 1 (avail - chromeWidth)) (columnWeights cfg)
      selRow = if activeTableIndex st == idx then rowPositions st !! idx else -1
      selCol = if activeTableIndex st == idx then colPositions st !! idx else -1

      headerWidgets = case columnHeaders cfg of
        Just hs -> [drawHeaderRow idx colWs hs, drawBorder idx colWs]
        Nothing -> []

      tableLines = concatMap (drawRow idx colWs (minColumnHeight cfg) (maxColumnHeight cfg) selRow selCol) (zip [0 ..] rows)
      allLines = headerWidgets ++ tableLines

      titled w = case title cfg of
        Just t -> vBox [withAttr (tableTitleAttr idx) (str t), padTop (Pad 1) w]
        Nothing -> w

  render $
    withAttr (borderAttr idx) $
      border $
        titled $
          vBox allLines

drawHeaderRow :: Int -> [Int] -> [String] -> Widget Name
drawHeaderRow idx colWs hs =
  hBox $
    zipWith
      ( \w h ->
          withAttr (headerAttr idx) (str " " <+> padRight (Pad (w - length h)) (str h))
            <+> withAttr (borderAttr idx) (str " |")
      )
      colWs
      (take (length colWs) hs ++ repeat "")

drawRow :: Int -> [Int] -> Int -> Int -> Int -> Int -> (Int, Row) -> [Widget Name]
drawRow tableIdx widths minH maxH selRow selCol (i, row) =
  let wrapped = zipWith (\w (txt, _) -> wrapOrTruncate w maxH txt) widths row
      rowHeight = max minH (maximum (1 : map length wrapped))
      padded = padCells rowHeight wrapped
      linesPerRow = transpose padded
   in map (drawLine tableIdx i row widths selRow selCol) linesPerRow ++ [drawBorder tableIdx widths]

drawLine :: Int -> Int -> Row -> [Int] -> Int -> Int -> [String] -> Widget Name
drawLine tableIdx i row widths selRow selCol line =
  if all null line
    then drawSpacerLine widths
    else hBox $ zipWith4 (drawCell tableIdx i row selRow selCol) [0 ..] line widths (repeat 1)

drawSpacerLine :: [Int] -> Widget Name
drawSpacerLine widths =
  str (replicate (sum (map (+ 3) widths)) ' ')

drawCell :: Int -> Int -> Row -> Int -> Int -> Int -> String -> Int -> Int -> Widget Name
drawCell tableIdx i row selRow selCol j txt w _ =
  let isSel = i == selRow && j == selCol
      hasLink = maybe False ((/= Nothing) . snd) (safeIndex row j)
      attr =
        if isSel
          then selectedTableAttr tableIdx
          else
            if hasLink
              then linkAttr tableIdx
              else textAttr tableIdx
      cell = withAttr attr (str " " <+> padRight (Pad (w - length txt)) (str txt))
      bar = withAttr (borderAttr tableIdx) (str " |")
   in if null txt && not isSel
        then drawBlankCell w
        else clickable (CellName tableIdx i j) cell <+> bar

drawBlankCell :: Int -> Widget Name
drawBlankCell w =
  str (replicate (w + 3) ' ')

drawBorder :: Int -> [Int] -> Widget Name
drawBorder idx widths =
  withAttr (borderAttr idx) $
    str "+" <+> hBox (map (\w -> str (replicate (w + 3) '-')) widths) <+> str "+"

wrapOrTruncate :: Int -> Int -> String -> [String]
wrapOrTruncate w h txt =
  let chunks = map T.unpack $ T.chunksOf w (T.pack txt)
   in if length chunks <= h
        then chunks
        else take (h - 1) chunks ++ [take (w - 3) (chunks !! (h - 1)) ++ "..."]

padCells :: Int -> [[String]] -> [[String]]
padCells h = map (\xs -> xs ++ replicate (h - length xs) "")

distributeWidths :: Int -> [Int] -> [Int]
distributeWidths total weights =
  let sumW = sum weights
   in map (\w -> max 1 $ w * total `div` sumW) weights

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 || i >= length xs = Nothing
  | otherwise = Just (xs !! i)

clampIndex :: Int -> Int -> Int -> Int
clampIndex lo hi x = max lo (min hi x)

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
  | tableCount st == 0 = st {activeTableIndex = 0, rowPositions = [], colPositions = []}
  | otherwise =
      let tableTotal = tableCount st
          activeIdx = clampIndex 0 (tableTotal - 1) (activeTableIndex st)
          fixedRows = take tableTotal (rowPositions st ++ repeat 0)
          fixedCols = take tableTotal (colPositions st ++ repeat 0)
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
              colPositions = map snd normalized
            }

moveSelection :: Int -> Int -> St -> St
moveSelection dRow dCol st =
  let normalized = normalizeSelection st
      tableIdx = activeTableIndex normalized
      rows = rowPositions normalized
      cols = colPositions normalized
      currentRow = rows !! tableIdx
      nextRowMax = rowCount normalized tableIdx - 1
      nextRow = if nextRowMax < 0 then 0 else clampIndex 0 nextRowMax (currentRow + dRow)
      nextColMax = colCount normalized tableIdx nextRow - 1
      currentCol = cols !! tableIdx
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

cellUrlAt :: St -> Int -> Int -> Int -> Maybe String
cellUrlAt st tableIdx rowIdx colIdx = do
  rows <- safeIndex (tableRowsData st) tableIdx
  row <- safeIndex rows rowIdx
  (_, mUrl) <- safeIndex row colIdx
  mUrl

selectedCellUrl :: St -> Maybe String
selectedCellUrl st = do
  normalized <- Just (normalizeSelection st)
  let tableIdx = activeTableIndex normalized
  rowIdx <- safeIndex (rowPositions normalized) tableIdx
  colIdx <- safeIndex (colPositions normalized) tableIdx
  cellUrlAt normalized tableIdx rowIdx colIdx

openUrl :: String -> IO Bool
openUrl target = do
  let command = case os of
        "mingw32" -> proc "powershell.exe" ["-NoProfile", "-Command", "Start-Process", target]
        "darwin" -> proc "open" [target]
        _ -> proc "xdg-open" [target]
  result <- try (void (createProcess command)) :: IO (Either IOException ())
  pure (either (const False) (const True) result)

handleEvent :: BrickEvent Name AppEvent -> EventM Name St ()
handleEvent (AppEvent (UpdateTable i rs)) =
  modify $ \st ->
    normalizeSelection st {tableRowsData = updateAt i (const rs) (tableRowsData st)}
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
              activeTableIndex = 0
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

app :: App St AppEvent Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = neverShowCursor,
      appHandleEvent = handleEvent,
      appStartEvent = pure (),
      appAttrMap = \st ->
        attrMap V.defAttr $
          tableAttrs (tables st)
    }

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

lookupFieldString :: String -> Object -> Maybe String
lookupFieldString key obj =
  case KM.lookup (K.fromString key) obj of
    Just (String t) -> Just (T.unpack t)
    Just (Number n) -> Just (show n)
    Just (Bool b) -> Just (show b)
    _ -> Nothing

valueToRow :: FieldMapping -> Value -> Maybe Row
valueToRow fm (Object obj) = do
  cells <- mapM (`lookupFieldString` obj) (fieldNames fm)
  pure (map (,Nothing) cells)
valueToRow _ _ = Nothing

fetchWebRows :: String -> FieldMapping -> IO [Row]
fetchWebRows endpoint fm = do
  req <- parseRequest endpoint
  resp <- httpLBS req
  let payload = getResponseBody resp
  pure (decodeRows payload fm)

fetchLocalRows :: FilePath -> FieldMapping -> IO [Row]
fetchLocalRows sourcePath fm = do
  result <- try (B.readFile sourcePath) :: IO (Either IOException B.ByteString)
  pure $ case result of
    Right payload -> decodeRows payload fm
    Left _ -> []

safeGetModificationTime :: FilePath -> IO (Maybe UTCTime)
safeGetModificationTime sourcePath = do
  result <- try (getModificationTime sourcePath) :: IO (Either IOException UTCTime)
  pure $ case result of
    Right modTime -> Just modTime
    Left _ -> Nothing

decodeRows :: B.ByteString -> FieldMapping -> [Row]
decodeRows payload fm =
  case eitherDecode payload :: Either String [Value] of
    Right values -> mapMaybe (valueToRow fm) values
    Left _ -> []

watchConfig :: FilePath -> BChan AppEvent -> MVar [ThreadId] -> IO ()
watchConfig path chan sourceThreadIds = do
  initialMod <- getModificationTime path
  loop initialMod
  where
    loop lastMod = do
      threadDelay 2_000_000
      newMod <- getModificationTime path

      nextMod <-
        if newMod > lastMod
          then do
            res <- B.readFile path
            case eitherDecode res of
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

refreshSourcesForLayout :: [LayoutItem] -> [(Int, TableSource)]
refreshSourcesForLayout items = zip [0 ..] (map source (flattenLayoutItems items))

startSourceThreads :: [(Int, TableSource)] -> BChan AppEvent -> IO [ThreadId]
startSourceThreads sources chan = fmap catMaybes (mapM forkSource sources)
  where
    forkSource (i, WebSource endpoint fm refresh) =
      Just
        <$> forkIO
          ( forever $ do
              rows <- fetchWebRows endpoint fm
              writeBChan chan (UpdateTable i rows)
              threadDelay (refresh * 1_000_000)
          )
    forkSource (i, LocalSource sourcePath fm refresh) =
      Just
        <$> forkIO
          ( do
              initialRows <- fetchLocalRows sourcePath fm
              writeBChan chan (UpdateTable i initialRows)
              initialMod <- safeGetModificationTime sourcePath
              loop initialMod refresh
          )
      where
        loop lastMod secondsUntilRefresh = do
          threadDelay 1_000_000
          currentMod <- safeGetModificationTime sourcePath
          if currentMod /= lastMod
            then do
              rows <- fetchLocalRows sourcePath fm
              writeBChan chan (UpdateTable i rows)
              loop currentMod refresh
            else
              if secondsUntilRefresh <= 1
                then do
                  rows <- fetchLocalRows sourcePath fm
                  writeBChan chan (UpdateTable i rows)
                  loop currentMod refresh
                else loop currentMod (secondsUntilRefresh - 1)
    forkSource _ = pure Nothing

main :: IO ()
main = do
  let cfgFile = "tables.json"
  file <- B.readFile cfgFile
  case eitherDecode file of
    Left err -> putStrLn ("Failed to load config: " ++ err)
    Right layoutCfgs -> do
      chan <- newBChan 10
      let flatTables = flattenLayoutItems layoutCfgs
          refreshSources = refreshSourcesForLayout layoutCfgs
      initialSourceThreadIds <- startSourceThreads refreshSources chan
      sourceThreadIds <- newMVar initialSourceThreadIds
      void $ forkIO $ watchConfig cfgFile chan sourceThreadIds
      let rows = initialRowsForLayout layoutCfgs
      let st =
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
