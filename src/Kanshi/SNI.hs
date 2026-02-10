module Kanshi.SNI
  ( startSNI
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (void, forever, when)
import Data.String (fromString)
import Data.Text (Text)
import DBus
import DBus.Client (connectSession, export, requestName, readOnlyProperty, Interface(..))
import qualified DBus.Client as DBus
import GI.Dbusmenu
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified GI.Gio.Objects.Cancellable as Gio
import qualified StatusNotifier.Watcher.Client as W
import System.Directory (getXdgDirectory, XdgDirectory(..), doesDirectoryExist)
import System.FSNotify (withManager, watchDir, Event(..))

import Hyprland.Monitors
import Kanshi.Config
import Kanshi.Varlink
import Menu

data SNIState = SNIState
  { sniConnection :: MVar (Maybe KanshiConnection)
  , sniAppState :: MVar AppState
  , sniMenuServer :: Server
  , sniGLibContext :: GLib.MainContext
  , sniDBusClient :: DBus.Client
  }

startSNI :: IO ()
startSNI = do
  let busName = "org.kanshi.SNI"
      path = "/StatusNotifierItem"
      menuPath = "/StatusNotifierItem/Menu"
      menuBusName = busName ++ ".Menu"

  client <- connectSession

  -- GLib main loop for gi-dbusmenu
  mainLoop <- GLib.mainLoopNew Nothing False >>= GLib.mainLoopRef
  context <- GLib.mainLoopGetContext mainLoop

  -- Menu server on Gio bus
  connection <- Just <$> Gio.cancellableNew >>= Gio.busGetSync Gio.BusTypeSession
  void $ Gio.busOwnNameOnConnection connection (toText menuBusName) [] Nothing Nothing
  menuServer <- serverNew (toText menuPath)

  -- Initial state
  kanshiConn <- tryConnect
  appState <- buildInitialState kanshiConn
  connVar <- newMVar kanshiConn
  stateVar <- newMVar appState

  let sniState = SNIState connVar stateVar menuServer context client

  -- Build initial menu
  rebuildMenu sniState

  -- SNI interface
  let clientInterface = Interface
        { interfaceName = "org.kde.StatusNotifierItem"
        , interfaceMethods = []
        , interfaceProperties =
            [ readOnlyProperty "IconName" (return ("video-display" :: String))
            , readOnlyProperty "Menu" (return $ objectPath_ menuPath)
            , readOnlyProperty "Category" (return ("Hardware" :: String))
            , readOnlyProperty "Id" (return ("kanshi-sni" :: String))
            , readOnlyProperty "Title" (return ("Display Profiles" :: String))
            ]
        , interfaceSignals = []
        }

  export client (fromString path) clientInterface
  requestName client (busName_ busName) []
  void $ W.registerStatusNotifierItem client busName

  -- Run GLib main loop in background thread
  void $ forkIO $ GLib.mainLoopRun mainLoop

  -- Watch kanshi config for changes
  configDir <- getXdgDirectory XdgConfig "kanshi"
  dirExists <- doesDirectoryExist configDir
  when dirExists $
    void $ forkIO $ withManager $ \mgr -> do
      void $ watchDir mgr configDir (const True) $ \event ->
        case event of
          Modified {} -> refreshState sniState
          Added {} -> refreshState sniState
          _ -> return ()
      forever $ threadDelay maxBound

  -- Block forever on main thread
  forever $ threadDelay maxBound

toText :: String -> Text
toText = fromString

tryConnect :: IO (Maybe KanshiConnection)
tryConnect = do
  result <- connectKanshi
  case result of
    Left _ -> return Nothing
    Right conn -> return $ Just conn

buildInitialState :: Maybe KanshiConnection -> IO AppState
buildInitialState mConn = do
  profiles <- either (const []) id <$> getProfileNames
  curProfile <- case mConn of
    Nothing -> return Nothing
    Just conn -> do
      result <- kanshiStatus conn
      return $ case result of
        Right status -> currentProfile status
        Left _ -> Nothing
  monitors <- either (const []) id <$> getMonitors
  return AppState
    { stateProfiles = profiles
    , stateCurrentProfile = curProfile
    , stateMonitors = monitors
    , stateKanshiConnected = case mConn of
        Nothing -> False
        Just _ -> True
    }

rebuildMenu :: SNIState -> IO ()
rebuildMenu sniState = do
  state <- readMVar (sniAppState sniState)
  let actions = MenuActions
        { onSwitchProfile = handleSwitchProfile sniState
        , onReloadConfig = handleReload sniState
        , onSetMode = handleSetMode sniState
        , onSetScale = handleSetScale sniState
        , onToggleMonitor = handleToggleMonitor sniState
        }
  newRoot <- buildMenu state actions
  runOnGLibMain (sniGLibContext sniState) $
    serverSetRoot (sniMenuServer sniState) newRoot

refreshState :: SNIState -> IO ()
refreshState sniState = do
  mConn <- readMVar (sniConnection sniState)
  newState <- buildInitialState mConn
  modifyMVar_ (sniAppState sniState) $ const $ return newState
  rebuildMenu sniState

runOnGLibMain :: GLib.MainContext -> IO () -> IO ()
runOnGLibMain context action =
  GLib.mainContextInvokeFull (Just context) 4 $
    action >> return False

handleSwitchProfile :: SNIState -> Text -> IO ()
handleSwitchProfile sniState profile = do
  mConn <- readMVar (sniConnection sniState)
  case mConn of
    Nothing -> return ()
    Just conn -> void $ kanshiSwitch conn profile
  refreshState sniState

handleReload :: SNIState -> IO ()
handleReload sniState = do
  mConn <- readMVar (sniConnection sniState)
  case mConn of
    Nothing -> do
      newConn <- tryConnect
      modifyMVar_ (sniConnection sniState) $ const $ return newConn
    Just conn -> void $ kanshiReload conn
  refreshState sniState

handleSetMode :: SNIState -> Text -> Text -> IO ()
handleSetMode sniState name mode = do
  setMonitorMode name mode
  refreshState sniState

handleSetScale :: SNIState -> Text -> Double -> IO ()
handleSetScale sniState name scale = do
  Hyprland.Monitors.setMonitorScale name scale
  refreshState sniState

handleToggleMonitor :: SNIState -> Text -> Bool -> IO ()
handleToggleMonitor sniState name currentlyEnabled = do
  if currentlyEnabled
    then disableMonitor name
    else enableMonitor name
  refreshState sniState
