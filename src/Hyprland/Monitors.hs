module Hyprland.Monitors
  ( MonitorInfo(..)
  , getMonitors
  , setMonitorMode
  , setMonitorScale
  , disableMonitor
  , enableMonitor
  ) where

import Control.Exception (try, IOException)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (readProcess, callProcess)

data MonitorInfo = MonitorInfo
  { monitorName :: Text
  , monitorDescription :: Text
  , monitorWidth :: Int
  , monitorHeight :: Int
  , monitorRefreshRate :: Double
  , monitorX :: Int
  , monitorY :: Int
  , monitorScale :: Double
  , monitorDisabled :: Bool
  , monitorFocused :: Bool
  , monitorAvailableModes :: [Text]
  , monitorTransform :: Int
  } deriving (Show, Eq)

instance FromJSON MonitorInfo where
  parseJSON = withObject "MonitorInfo" $ \v -> MonitorInfo
    <$> v .: "name"
    <*> v .: "description"
    <*> v .: "width"
    <*> v .: "height"
    <*> v .: "refreshRate"
    <*> v .: "x"
    <*> v .: "y"
    <*> v .: "scale"
    <*> v .: "disabled"
    <*> v .: "focused"
    <*> v .: "availableModes"
    <*> v .: "transform"

-- | Query all monitors via hyprctl
getMonitors :: IO (Either String [MonitorInfo])
getMonitors = do
  result <- try $ readProcess "hyprctl" ["monitors", "-j"] ""
  case (result :: Either IOException String) of
    Left err -> return $ Left $ "hyprctl failed: " ++ show err
    Right output -> return $ eitherDecode $ LBS.pack output

-- | Run a hyprctl keyword command, catching exceptions
hyprctlKeyword :: Text -> IO (Either String ())
hyprctlKeyword args = do
  result <- try $ callProcess "hyprctl" ["keyword", "monitor", T.unpack args]
  return $ case (result :: Either IOException ()) of
    Left err -> Left $ "hyprctl failed: " ++ show err
    Right () -> Right ()

-- | Set monitor to a specific mode string like "2560x1600@240Hz"
setMonitorMode :: Text -> Text -> IO (Either String ())
setMonitorMode name mode =
  hyprctlKeyword $ name <> "," <> mode <> ",auto,1"

-- | Set monitor scale
setMonitorScale :: Text -> Double -> IO (Either String ())
setMonitorScale name scale =
  hyprctlKeyword $ name <> ",preferred,auto," <> T.pack (show scale)

-- | Disable a monitor
disableMonitor :: Text -> IO (Either String ())
disableMonitor name =
  hyprctlKeyword $ name <> ",disabled"

-- | Re-enable a monitor with preferred settings
enableMonitor :: Text -> IO (Either String ())
enableMonitor name =
  hyprctlKeyword $ name <> ",preferred,auto,1"
