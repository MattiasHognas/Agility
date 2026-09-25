-- | Loading media for image tables. Fetching and decoding happen on the
-- source thread; drawing only scales and renders an already decoded 'Frame'.
-- Video will fit the same shape: a thread that sends a new frame per tick.
module Agility.Media
  ( decodeImage,
    loadImageUrl,
  )
where

import           Agility.Media.Frame  (Frame)
import           Agility.Media.Png    (decodePng, isPng)
import           Agility.Media.Scale  (limitFrame)
import           Agility.Types        (MediaState (..))
import           Control.Exception    (SomeAsyncException, SomeException,
                                       evaluate, fromException, throwIO, try)
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Network.HTTP.Simple  (HttpException (..), getResponseBody,
                                       getResponseStatusCode, httpLBS,
                                       parseRequest)

-- | Decoded frames larger than this are shrunk once up front; no terminal
-- table needs more pixels than this.
maxDecodedSize :: (Int, Int)
maxDecodedSize = (1024, 1024)

-- | Pick a decoder from the file's leading bytes.
decodeImage :: B.ByteString -> Either String Frame
decodeImage bytes
  | isPng bytes = decodePng bytes
  | otherwise = Left "unsupported image format (only PNG is supported)"

loadImageUrl :: String -> IO MediaState
loadImageUrl url = do
  result <- try fetchAndDecode
  case result of
    Right media -> pure media
    Left err
      | Just (_ :: SomeAsyncException) <- fromException err -> throwIO err
      | Just httpErr <- fromException err -> pure (MediaFailed (describeHttpError httpErr))
      | otherwise -> pure (MediaFailed (show (err :: SomeException)))
  where
    fetchAndDecode = do
      response <- httpLBS =<< parseRequest url
      let status = getResponseStatusCode response
      if status < 200 || status >= 300
        then pure (MediaFailed ("HTTP " ++ show status))
        else case decodeImage (L.toStrict (getResponseBody response)) of
          Left err -> pure (MediaFailed err)
          Right frame ->
            MediaReady <$> evaluate (uncurry limitFrame maxDecodedSize frame)

describeHttpError :: HttpException -> String
describeHttpError (HttpExceptionRequest _ content) = "could not fetch image: " ++ takeWhile (/= ' ') (show content)
describeHttpError (InvalidUrlException _ reason) = "invalid image url: " ++ reason
