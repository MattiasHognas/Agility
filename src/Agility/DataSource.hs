module Agility.DataSource
  ( decodeRows,
    fetchLocalRows,
    fetchWebRows,
    safeGetModificationTime,
  )
where

import Agility.Types (FieldMapping (fieldNames), Row)
import Control.Exception (IOException, try)
import Data.Aeson (Object, Value (..), eitherDecode)
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as B
import Data.Maybe (mapMaybe)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Network.HTTP.Simple (getResponseBody, httpLBS, parseRequest)
import System.Directory (getModificationTime)

lookupFieldString :: String -> Object -> Maybe String
lookupFieldString key obj =
  case KM.lookup (K.fromString key) obj of
    Just (String txt) -> Just (T.unpack txt)
    Just (Number n) -> Just (show n)
    Just (Bool b) -> Just (show b)
    _ -> Nothing

valueToRow :: FieldMapping -> Value -> Maybe Row
valueToRow fm (Object obj) = do
  cells <- mapM (`lookupFieldString` obj) (fieldNames fm)
  pure (map (,Nothing) cells)
valueToRow _ _ = Nothing

decodeRows :: B.ByteString -> FieldMapping -> [Row]
decodeRows payload fm =
  case eitherDecode payload :: Either String [Value] of
    Right values -> mapMaybe (valueToRow fm) values
    Left _ -> []

fetchWebRows :: String -> FieldMapping -> IO [Row]
fetchWebRows endpoint fm = do
  req <- parseRequest endpoint
  resp <- httpLBS req
  pure (decodeRows (getResponseBody resp) fm)

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
