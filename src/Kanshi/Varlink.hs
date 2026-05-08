module Kanshi.Varlink
  ( KanshiConnection
  , connectKanshi
  , disconnectKanshi
  , kanshiStatus
  , kanshiSwitch
  , kanshiReload
  , KanshiStatus(..)
  , KanshiError(..)
  ) where

import Control.Exception (try, IOException)
import Data.Aeson
import Data.Aeson.Key (fromText)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (lookupEnv)
import System.Posix.Types (CUid(..))
import System.Posix.User (getEffectiveUserID)

newtype KanshiConnection = KanshiConnection Socket

data KanshiStatus = KanshiStatus
  { currentProfile :: Maybe Text
  , pendingProfile :: Maybe Text
  } deriving (Show)

data KanshiError
  = ConnectionFailed String
  | ProtocolError String
  | ProfileNotFound
  | ProfileNotMatched
  | ProfileNotApplied
  deriving (Show)

getSocketPath :: IO (Either KanshiError FilePath)
getSocketPath = do
  CUid uid <- getEffectiveUserID
  mDisplay <- lookupEnv "WAYLAND_DISPLAY"
  case mDisplay of
    Nothing -> return $ Left $ ConnectionFailed "WAYLAND_DISPLAY not set"
    Just display ->
      return $ Right $ "/run/user/" ++ show uid ++ "/fr.emersion.kanshi." ++ display

connectKanshi :: IO (Either KanshiError KanshiConnection)
connectKanshi = do
  ePath <- getSocketPath
  case ePath of
    Left err -> return $ Left err
    Right path -> do
      result <- try $ do
        sock <- socket AF_UNIX Stream defaultProtocol
        connect sock (SockAddrUnix path)
        return sock
      case (result :: Either IOException Socket) of
        Left e ->
          return $ Left $ ConnectionFailed $ "Could not connect: " ++ show e
        Right sock ->
          return $ Right $ KanshiConnection sock

disconnectKanshi :: KanshiConnection -> IO ()
disconnectKanshi (KanshiConnection sock) = close sock

varlinkCall :: KanshiConnection -> Value -> IO (Either KanshiError Value)
varlinkCall (KanshiConnection sock) request = do
  result <- try $ do
    sendAll sock $ LBS.toStrict (encode request) <> BS.singleton 0
    recvUntilNull sock
  case (result :: Either IOException BS.ByteString) of
    Left err ->
      return $ Left $ ConnectionFailed $ "Socket I/O failed: " ++ show err
    Right response ->
      case eitherDecodeStrict response of
        Left err -> return $ Left $ ProtocolError $ "JSON decode error: " ++ err
        Right val -> return $ parseVarlinkResponse val

recvUntilNull :: Socket -> IO BS.ByteString
recvUntilNull sock = go BS.empty
  where
    go acc = do
      chunk <- recv sock 4096
      if BS.null chunk
        then return acc
        else
          let (before, after) = BS.break (== 0) chunk
              acc' = acc <> before
          in if BS.null after
             then go acc'
             else return acc'

parseVarlinkResponse :: Value -> Either KanshiError Value
parseVarlinkResponse (Object obj)
  | Just (String errName) <- KM.lookup (fromText "error") obj =
      Left $ case errName of
        "fr.emersion.kanshi.ProfileNotFound" -> ProfileNotFound
        "fr.emersion.kanshi.ProfileNotMatched" -> ProfileNotMatched
        "fr.emersion.kanshi.ProfileNotApplied" -> ProfileNotApplied
        other -> ProtocolError $ "Varlink error: " ++ T.unpack other
  | otherwise = Right (Object obj)
parseVarlinkResponse v = Right v

kanshiStatus :: KanshiConnection -> IO (Either KanshiError KanshiStatus)
kanshiStatus conn = do
  result <- varlinkCall conn $ object
    [ "method" .= ("fr.emersion.kanshi.Status" :: Text) ]
  return $ case result of
    Left err -> Left err
    Right (Object obj) ->
      let params = case KM.lookup (fromText "parameters") obj of
            Just (Object p) -> p
            _ -> KM.empty
          getCurrent = case KM.lookup (fromText "current_profile") params of
            Just (String s) -> Just s
            _ -> Nothing
          getPending = case KM.lookup (fromText "pending_profile") params of
            Just (String s) -> Just s
            _ -> Nothing
      in Right $ KanshiStatus getCurrent getPending
    Right _ -> Left $ ProtocolError "Unexpected response format"

kanshiSwitch :: KanshiConnection -> Text -> IO (Either KanshiError ())
kanshiSwitch conn profile = do
  result <- varlinkCall conn $ object
    [ "method" .= ("fr.emersion.kanshi.Switch" :: Text)
    , "parameters" .= object [ "profile" .= profile ]
    ]
  return $ case result of
    Left err -> Left err
    Right _ -> Right ()

kanshiReload :: KanshiConnection -> IO (Either KanshiError ())
kanshiReload conn = do
  result <- varlinkCall conn $ object
    [ "method" .= ("fr.emersion.kanshi.Reload" :: Text) ]
  return $ case result of
    Left err -> Left err
    Right _ -> Right ()
