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

-- | Set monitor to a specific mode string like "2560x1600@240Hz"
setMonitorMode :: Text -> Text -> IO ()
setMonitorMode name mode =
  callProcess "hyprctl" ["keyword", "monitor",
    T.unpack $ name <> "," <> mode <> ",auto,1"]

-- | Set monitor scale
setMonitorScale :: Text -> Double -> IO ()
setMonitorScale name scale =
  callProcess "hyprctl" ["keyword", "monitor",
    T.unpack $ name <> ",preferred,auto," <> T.pack (show scale)]

-- | Disable a monitor
disableMonitor :: Text -> IO ()
disableMonitor name =
  callProcess "hyprctl" ["keyword", "monitor",
    T.unpack $ name <> ",disabled"]

-- | Re-enable a monitor with preferred settings
enableMonitor :: Text -> IO ()
enableMonitor name =
  callProcess "hyprctl" ["keyword", "monitor",
    T.unpack $ name <> ",preferred,auto,1"]
