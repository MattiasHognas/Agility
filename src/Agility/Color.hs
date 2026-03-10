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

import Agility.Types (ColorConfig (..), TableConfig (..), parseColor)
import Brick (AttrName, attrName, fg)
import Control.Applicative ((<|>))
import Graphics.Vty qualified as V

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
