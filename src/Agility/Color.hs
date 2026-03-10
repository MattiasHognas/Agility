module Agility.Color
  ( borderAttr,
    headerAttr,
    linkAttr,
    mkAttr,
    parseColor,
    selectedTableAttr,
    tableAttrs,
    tableTitleAttr,
    textAttr,
  )
where

import Agility.Types (ColorConfig (..), TableConfig (..))
import Brick (AttrName, attrName, fg)
import Control.Applicative ((<|>))
import Data.Char (toLower)
import Graphics.Vty qualified as V
import Numeric (readHex)

textAttr :: Int -> AttrName
textAttr idx = attrName ("table" ++ show idx ++ ".text")

borderAttr :: Int -> AttrName
borderAttr idx = attrName ("table" ++ show idx ++ ".border")

headerAttr :: Int -> AttrName
headerAttr idx = attrName ("table" ++ show idx ++ ".header")

linkAttr :: Int -> AttrName
linkAttr idx = attrName ("table" ++ show idx ++ ".link")

tableTitleAttr :: Int -> AttrName
tableTitleAttr idx = attrName ("table" ++ show idx ++ ".title")

selectedTableAttr :: Int -> AttrName
selectedTableAttr idx = attrName ("table" ++ show idx ++ ".selected")

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

parseColor :: String -> Maybe V.Color
parseColor value = parseNamedColor value <|> parseHexColor value

mkAttr :: Maybe String -> Maybe String -> V.Attr
mkAttr mFg mBg =
  let base = V.defAttr
      withForeground = maybe base (\value -> maybe base (\colorValue -> V.withForeColor base colorValue) (parseColor value)) mFg
   in maybe withForeground (\value -> maybe withForeground (\colorValue -> V.withBackColor withForeground colorValue) (parseColor value)) mBg

tableAttrs :: [TableConfig] -> [(AttrName, V.Attr)]
tableAttrs cfgs =
  concatMap one (zip [0 ..] cfgs)
  where
    one (idx, cfg) =
      let colorCfg = colors cfg
          txt = colorCfg >>= textColor
          bord = colorCfg >>= borderColor
          ttl = colorCfg >>= titleColor
          hdr = colorCfg >>= headerColor
          selFg = colorCfg >>= selectedTextColor
          selBg = colorCfg >>= selectedBgColor
       in [ (textAttr idx, mkAttr txt Nothing),
            (borderAttr idx, mkAttr bord Nothing),
            (headerAttr idx, mkAttr hdr Nothing),
            (linkAttr idx, V.withStyle (mkAttr (txt <|> hdr <|> Just "cyan") Nothing) V.underline),
            (tableTitleAttr idx, V.withStyle (mkAttr ttl Nothing) V.bold),
            ( selectedTableAttr idx,
              case selFg <|> selBg of
                Nothing -> fg V.yellow
                Just _ -> mkAttr selFg selBg
            )
          ]
