module Agility.Config
  ( decodeLayoutConfig,
  )
where

import Agility.Types (LayoutItem)
import Data.Aeson (eitherDecode)
import Data.ByteString.Lazy (ByteString)

decodeLayoutConfig :: ByteString -> Either String [LayoutItem]
decodeLayoutConfig = eitherDecode
