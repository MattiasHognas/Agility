{-# LANGUAGE OverloadedStrings, DeriveGeneric, NumericUnderscores #-}

import Brick hiding (Horizontal, Vertical)
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import Control.Concurrent
import Control.Monad (forever, void)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Simple
import GHC.Generics
import Data.Aeson
import Data.Maybe (mapMaybe)
import Data.List (transpose, zipWith4)
import System.Directory (getModificationTime)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Char (toLower)
import Numeric (readHex)

type Name = ()
type Cell = (String, Maybe String)
type Row = [Cell]

data TablePlacement = Horizontal | Vertical deriving (Show, Read, Eq)

data TableSource
  = StaticSource [[Cell]]
  | RestSource
      { url :: String
      , fields :: FieldMapping
      , refreshSeconds :: Int
      }
  deriving Show

data FieldMapping = FieldMapping
  { titleField :: String
  , bodyField  :: String
  , idField    :: String
  } deriving (Show, Generic)

data ColorConfig = ColorConfig
  { textColor         :: Maybe String
  , borderColor       :: Maybe String
  , titleColor        :: Maybe String
  , headerColor       :: Maybe String
  , selectedTextColor :: Maybe String
  , selectedBgColor   :: Maybe String
  } deriving Show

data TableConfig = TableConfig
  { title         :: Maybe String
  , columnHeaders :: Maybe [String]
  , placement     :: TablePlacement
  , columnWeights :: [Int]
  , columnHeights :: [Int]
  , maxWidth      :: Maybe Int
  , colors        :: Maybe ColorConfig
  , source        :: TableSource
  } deriving Show

data St = St
  { activeTableIndex :: Int
  , rowPositions     :: [Int]
  , colPositions     :: [Int]
  , tables           :: [TableConfig]
  , tableRowsData    :: [[Row]]
  } deriving Show

data AppEvent
  = UpdateTable Int [Row]
  | ReloadConfig [TableConfig]
  deriving Show

textAttr :: Int -> AttrName
textAttr i = attrName ("table" ++ show i ++ ".text")

borderAttr :: Int -> AttrName
borderAttr i = attrName ("table" ++ show i ++ ".border")

headerAttr :: Int -> AttrName
headerAttr i = attrName ("table" ++ show i ++ ".header")

tableTitleAttr :: Int -> AttrName
tableTitleAttr i = attrName ("table" ++ show i ++ ".title")

selectedTableAttr :: Int -> AttrName
selectedTableAttr i = attrName ("table" ++ show i ++ ".selected")

instance FromJSON TablePlacement where
  parseJSON = withText "TablePlacement" $ \t ->
    case T.toLower t of
      "horizontal" -> pure Horizontal
      "vertical"   -> pure Vertical
      _ -> fail "placement must be 'horizontal' or 'vertical'"

instance FromJSON FieldMapping where
  parseJSON = withObject "FieldMapping" $ \v ->
    FieldMapping <$> v .: "title" <*> v .: "body" <*> v .: "id"

parseValidColor :: Object -> String -> Parser (Maybe String)
parseValidColor v keyStr = do
  ms <- v .:? K.fromString keyStr
  case ms of
    Nothing -> pure Nothing
    Just s  -> case parseColor s of
      Just _  -> pure (Just s)
      Nothing -> fail $
        "Invalid color value for '" ++ keyStr ++ "': \"" ++ s ++
        "\". Use a named color (e.g. \"red\", \"blue\") or a hex value (e.g. \"#ff0000\")."

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
            parseCell [txt]       = (txt, Nothing)
            parseCell _           = ("", Nothing)
        pure $ StaticSource (map (map parseCell) rawRows)
      "rest" -> RestSource
        <$> v .: "url"
        <*> v .: "fields"
        <*> v .:? "refreshSeconds" .!= 5
      _ -> fail "Unknown source type"

instance FromJSON TableConfig where
  parseJSON = withObject "TableConfig" $ \v ->
    TableConfig <$> v .:? "title"
                <*> v .:? "columnHeaders"
                <*> v .:  "placement"
                <*> v .:  "columnWeights"
                <*> v .:  "columnHeights"
                <*> v .:? "maxWidth"
                <*> v .:? "colors"
                <*> v .:  "source"

parseNamedColor :: String -> Maybe V.Color
parseNamedColor s =
  case map toLower s of
    "black"   -> Just V.black
    "red"     -> Just V.red
    "green"   -> Just V.green
    "yellow"  -> Just V.yellow
    "blue"    -> Just V.blue
    "magenta" -> Just V.magenta
    "cyan"    -> Just V.cyan
    "white"   -> Just V.white
    _         -> Nothing

parseHexColor :: String -> Maybe V.Color
parseHexColor ('#':xs)
  | length xs == 6 =
      case mapM read2 [take 2 xs, take 2 (drop 2 xs), take 2 (drop 4 xs)] of
        Just [r, g, b] -> Just (V.rgbColor r g b)
        _              -> Nothing
  where
    read2 h = case readHex h of
      [(n, "")] -> Just n
      _         -> Nothing
parseHexColor _ = Nothing

parseColor :: String -> Maybe V.Color
parseColor s = parseNamedColor s <|> parseHexColor s

mkAttr :: Maybe String -> Maybe String -> V.Attr
mkAttr mFg mBg =
  let base  = V.defAttr
      withF = maybe base (\s -> maybe base (`fg` base) (parseColor s)) mFg
  in maybe withF (\s -> maybe withF (`bg` withF) (parseColor s)) mBg

tableAttrs :: [TableConfig] -> [(AttrName, V.Attr)]
tableAttrs cfgs =
  concatMap one (zip [0 ..] cfgs)
  where
    one (i, cfg) =
      let c     = colors cfg
          txt   = c >>= textColor
          bord  = c >>= borderColor
          ttl   = c >>= titleColor
          hdr   = c >>= headerColor
          selFg = c >>= selectedTextColor
          selBg = c >>= selectedBgColor
      in
        [ (textAttr i,         mkAttr txt Nothing)
        , (borderAttr i,       mkAttr bord Nothing)
        , (headerAttr i,       mkAttr hdr Nothing)
        , (tableTitleAttr i,   V.withStyle (mkAttr ttl Nothing) V.bold)
        , (selectedTableAttr i, case selFg <|> selBg of
                                  Nothing -> fg V.yellow
                                  Just _  -> mkAttr selFg selBg)
        ]

drawUI :: St -> [Widget Name]
drawUI st = [center $ layoutTables st (tables st) (tableRowsData st) 0]

layoutTables :: St -> [TableConfig] -> [[Row]] -> Int -> Widget Name
layoutTables _ [] _ _ = emptyWidget
layoutTables st (t:ts) (r:rs) idx =
  let w = drawTable st idx t r
      rest = layoutTables st ts rs (idx + 1)
  in case placement t of
       Horizontal -> hBox [w, padLeft (Pad 2) rest]
       Vertical   -> vBox [w, padTop (Pad 1) rest]
layoutTables _ _ _ _ = emptyWidget

drawTable :: St -> Int -> TableConfig -> [Row] -> Widget Name
drawTable st idx cfg rows = Widget Fixed Fixed $ do
  ctx <- getContext
  let avail   = availWidth ctx
      usable  = maybe avail (min avail) (maxWidth cfg)
      colWs   = distributeWidths usable (columnWeights cfg)
      heights = columnHeights cfg
      selRow  = if activeTableIndex st == idx then rowPositions st !! idx else -1
      selCol  = if activeTableIndex st == idx then colPositions st !! idx else -1

      headerWidgets = case columnHeaders cfg of
        Just hs -> [drawHeaderRow idx colWs hs, drawBorder idx colWs]
        Nothing -> []

      tableLines = concatMap (drawRow idx colWs heights selRow selCol) (zip [0..] rows)
      allLines   = headerWidgets ++ tableLines

      titled w = case title cfg of
        Just t  -> vBox [withAttr (tableTitleAttr idx) (str t), padTop (Pad 1) w]
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
      (\w h ->
        withAttr (headerAttr idx) (padRight (Pad (w - length h)) (str h))
        <+> withAttr (borderAttr idx) (str " |"))
      colWs
      (take (length colWs) hs ++ repeat "")

drawRow :: Int -> [Int] -> [Int] -> Int -> Int -> (Int, Row) -> [Widget Name]
drawRow tableIdx widths heights selRow selCol (i, row) =
  let wrapped     = zipWith3 (\w h (txt, _) -> wrapOrTruncate w h txt) widths heights row
      padded      = padCells (maximum heights) wrapped
      linesPerRow = transpose padded
  in map (drawLine tableIdx i row widths selRow selCol) linesPerRow ++ [drawBorder tableIdx widths]

drawLine :: Int -> Int -> Row -> [Int] -> Int -> Int -> [String] -> Widget Name
drawLine tableIdx i row widths selRow selCol line =
  hBox $ zipWith4 (drawCell tableIdx i row selRow selCol) [0..] line widths (repeat 1)

drawCell :: Int -> Int -> Row -> Int -> Int -> Int -> String -> Int -> Int -> Widget Name
drawCell tableIdx i _ selRow selCol j txt w _ =
  let isSel    = i == selRow && j == selCol
      cellAttr = if isSel then withAttr (selectedTableAttr tableIdx)
                         else withAttr (textAttr tableIdx)
      bar      = withAttr (borderAttr tableIdx) (str " |")
  in cellAttr (str " " <+> padRight (Pad (w - length txt)) (str txt)) <+> bar

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
  in map (\w -> max 10 $ w * total `div` sumW) weights

handleEvent :: BrickEvent Name AppEvent -> EventM Name St ()
handleEvent (AppEvent (UpdateTable i rs)) =
  modify $ \st ->
    st { tableRowsData = updateAt i (const rs) (tableRowsData st) }

handleEvent (AppEvent (ReloadConfig cfgs)) =
  let rows = map (\cfg -> case source cfg of
               StaticSource rs -> rs
               _               -> []) cfgs
  in modify $ \st ->
       st
         { tables = cfgs
         , tableRowsData = rows
         , rowPositions = replicate (length cfgs) 0
         , colPositions = replicate (length cfgs) 0
         , activeTableIndex = 0
         }

handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent _ = pure ()

app :: App St AppEvent Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = \st -> attrMap V.defAttr $
      tableAttrs (tables st)
  }

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs = take i xs ++ [f (xs !! i)] ++ drop (i + 1) xs

lookupFieldString :: String -> Object -> Maybe String
lookupFieldString key obj =
  case KM.lookup (K.fromString key) obj of
    Just (String t) -> Just (T.unpack t)
    Just (Number n) -> Just (show n)
    Just (Bool b)   -> Just (show b)
    _               -> Nothing

valueToRow :: FieldMapping -> Value -> Maybe Row
valueToRow fm (Object obj) = do
  titleTxt <- lookupFieldString (titleField fm) obj
  bodyTxt  <- lookupFieldString (bodyField fm) obj
  identTxt <- lookupFieldString (idField fm) obj
  pure [(titleTxt, Nothing), (bodyTxt, Nothing), (identTxt, Nothing)]
valueToRow _ _ = Nothing

fetchRestRows :: String -> FieldMapping -> IO [Row]
fetchRestRows endpoint fm = do
  req <- parseRequest endpoint
  resp <- httpLBS req
  let payload = getResponseBody resp
  case eitherDecode payload :: Either String [Value] of
    Right values -> pure (mapMaybe (valueToRow fm) values)
    Left _       -> pure []

watchConfig :: FilePath -> BChan AppEvent -> IO ()
watchConfig path chan = do
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
                writeBChan chan (ReloadConfig cfg)
                pure newMod
              Left err -> do
                putStrLn $ "JSON parse error: " ++ err
                pure lastMod
          else pure lastMod

      loop nextMod

startRestThreads :: [(Int, TableSource)] -> BChan AppEvent -> IO ()
startRestThreads sources chan = mapM_ forkSource sources
  where
    forkSource (i, RestSource endpoint fm refresh) =
      void $ forkIO $ forever $ do
        rows <- fetchRestRows endpoint fm
        writeBChan chan (UpdateTable i rows)
        threadDelay (refresh * 1_000_000)
    forkSource _ = pure ()

main :: IO ()
main = do
  let cfgFile = "tables.json"
  file <- B.readFile cfgFile
  case eitherDecode file of
    Left err -> putStrLn ("Failed to load config: " ++ err)
    Right tableCfgs -> do
      chan <- newBChan 10
      void $ forkIO $ watchConfig cfgFile chan
      startRestThreads (zip [0..] (map source tableCfgs)) chan
      rows <- mapM (\cfg -> case source cfg of
        StaticSource rs -> pure rs
        _               -> pure []) tableCfgs
      let st = St
            0
            (replicate (length tableCfgs) 0)
            (replicate (length tableCfgs) 0)
            tableCfgs
            rows
      void $ defaultMain app st
