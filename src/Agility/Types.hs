{-# LANGUAGE OverloadedStrings #-}

module Agility.Types where

import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Aeson
  ( FromJSON (parseJSON),
    Object,
    Value (Object),
    withObject,
    (.!=),
    (.:),
    (.:?),
  )
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import GHC.Generics (Generic)
import Graphics.Vty qualified as V
import Numeric (readHex)

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

instance FromJSON FieldMapping where
  parseJSON value =
    (FieldMapping <$> parseJSON value)
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
        value

parseValidColor :: Object -> String -> Parser (Maybe String)
parseValidColor obj keyStr = do
  mValue <- obj .:? K.fromString keyStr
  case mValue of
    Nothing -> pure Nothing
    Just value -> case parseConfigColor value of
      Just _ -> pure (Just value)
      Nothing ->
        fail $
          "Invalid color value for '"
            ++ keyStr
            ++ "': \""
            ++ value
            ++ "\". Use a named color (e.g. \"red\", \"blue\") or a hex value (e.g. \"#ff0000\")."

instance FromJSON ColorConfig where
  parseJSON = withObject "ColorConfig" $ \obj ->
    ColorConfig
      <$> parseValidColor obj "text"
      <*> parseValidColor obj "border"
      <*> parseValidColor obj "title"
      <*> parseValidColor obj "header"
      <*> parseValidColor obj "selectedText"
      <*> parseValidColor obj "selectedBg"

instance FromJSON TableSource where
  parseJSON = withObject "TableSource" $ \obj -> do
    typ <- obj .: "type"
    case (typ :: String) of
      "static" -> do
        rawRows <- obj .: "rows"
        let parseCell [txt, mUrl] = (txt, Just mUrl)
            parseCell [txt] = (txt, Nothing)
            parseCell _ = ("", Nothing)
        pure $ StaticSource (map (map parseCell) rawRows)
      "web" -> do
        refresh <- parseRefreshSeconds obj
        WebSource
          <$> obj .: "url"
          <*> obj .: "fields"
          <*> pure refresh
      "local" -> do
        refresh <- parseRefreshSeconds obj
        LocalSource
          <$> obj .: "path"
          <*> obj .: "fields"
          <*> pure refresh
      _ -> fail "Unknown source type"

parseRefreshSeconds :: Object -> Parser Int
parseRefreshSeconds obj = do
  refresh <- obj .:? "refreshSeconds" .!= 5
  when (refresh <= 0) $ fail "refreshSeconds must be at least 1"
  pure refresh

instance FromJSON TableConfig where
  parseJSON = withObject "TableConfig" $ \obj -> do
    minH <- obj .:? "minColumnHeight" .!= 1
    maxH <- obj .:? "maxColumnHeight" .!= minH
    when (minH < 1) $ fail "minColumnHeight must be at least 1"
    when (maxH < minH) $ fail "maxColumnHeight must be greater than or equal to minColumnHeight"
    weights <- obj .: "columnWeights"
    when (null weights) $ fail "columnWeights must not be empty"
    when (any (<= 0) weights) $ fail "all columnWeights must be positive (greater than 0)"
    TableConfig
      <$> obj .:? "title"
      <*> obj .:? "columnHeaders"
      <*> pure weights
      <*> pure minH
      <*> pure maxH
      <*> obj .:? "colors"
      <*> obj .: "source"

instance FromJSON LayoutItem where
  parseJSON = withObject "LayoutItem" $ \obj ->
    if KM.member (K.fromString "tableWeights") obj || KM.member (K.fromString "tables") obj
      then parseGroup obj
      else TableItem <$> parseJSON (Object obj)
    where
      parseGroup obj = do
        weights <- obj .: "tableWeights"
        when (null weights) $ fail "tableWeights must not be empty"
        when (any (<= 0) weights) $ fail "all tableWeights must be positive (greater than 0)"
        cfgs <- obj .: "tables"
        when (null cfgs) $ fail "horizontal group must include at least one table"
        when (length weights /= length cfgs) $ fail "tableWeights must match the number of tables"
        pure (HorizontalGroup weights cfgs)

parseConfigColor :: String -> Maybe V.Color
parseConfigColor value = parseNamedColor value <|> parseHexColor value

parseNamedColor :: String -> Maybe V.Color
parseNamedColor value =
  case map toLower value of
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
        Just [red, green, blue] -> Just (V.rgbColor red green blue)
        _ -> Nothing
  where
    parseHexByte :: String -> Maybe Int
    parseHexByte hexValue = case readHex hexValue of
      [(n, "")] -> Just n
      _ -> Nothing
parseHexColor _ = Nothing
