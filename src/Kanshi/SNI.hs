module Kanshi.SNI
  ( startSNI
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception (SomeException, catch)
import Control.Monad (forever, void, when)
import Data.Int (Int32)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import DBus
import DBus.Client (Client, connectSession, export, requestName, readOnlyProperty, Interface(..))
import qualified GI.Dbusmenu as Dbusmenu
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified GI.Gio.Objects.Cancellable as Gio
import qualified Data.GI.Base as GI
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr, FunPtr)
import Foreign.StablePtr
  ( StablePtr
  , newStablePtr
  , deRefStablePtr
  , freeStablePtr
  , castStablePtrToPtr
  , castPtrToStablePtr
  )
import System.Directory (XdgDirectory(..), doesDirectoryExist, getXdgDirectory)
import System.FSNotify (Event(..), withManager, watchDir)
import System.IO.Unsafe (unsafePerformIO)

import qualified StatusNotifier.Watcher.Client as W

import DBus.Proxy (proxyAll)
import Hyprland.Monitors
import Kanshi.Config
import Kanshi.Varlink
import Menu

data SNIState = SNIState
  { sniConnection :: MVar (Maybe KanshiConnection)
  , sniAppState :: MVar AppState
  , sniMenuServer :: Dbusmenu.Server
  , sniGLibContext :: GLib.MainContext
  }

startSNI :: IO ()
startSNI = do
  let busName = "org.kanshi.SNI"
      path = "/StatusNotifierItem"
      menuPath = path ++ "/Menu"
      menuBusName = busName ++ ".Menu"

  -- DBus connection used for exporting the SNI and for proxying the menu onto
  -- the item bus.
  client <- connectSession

  -- Menu is hosted via gi-dbusmenu on a separate bus name, then proxied onto
  -- the item bus name (see DBus.Proxy).
  menuConnection <- Just <$> Gio.cancellableNew >>= Gio.busGetSync Gio.BusTypeSession
  void $ Gio.busOwnNameOnConnection menuConnection (T.pack menuBusName) [] Nothing Nothing
  menuServer <- Dbusmenu.serverNew (T.pack menuPath)

  -- GLib main loop needed by gi-dbusmenu.
  mainLoop <- GLib.mainLoopNew Nothing False >>= GLib.mainLoopRef
  context <- GLib.mainLoopGetContext mainLoop
  void $ forkIO $ GLib.mainLoopRun mainLoop

  -- Initial state
  kanshiConn <- tryConnect
  appState <- buildInitialState kanshiConn
  connVar <- newMVar kanshiConn
  stateVar <- newMVar appState

  let sniState = SNIState connVar stateVar menuServer context

  -- Build initial menu
  rebuildMenu sniState

  -- Export SNI on the main bus name.
  exportSNI client busName path menuPath
  _ <- requestName client (busName_ busName) []

  -- Proxy the menu from menuBusName onto busName at the same object path, so
  -- hosts can find com.canonical.dbusmenu at busName:/StatusNotifierItem/Menu.
  proxyAll client (busName_ menuBusName) (objectPath_ menuPath) (objectPath_ menuPath)

  -- Register with the watcher (must be done from the same DBus connection that
  -- owns the SNI bus name).
  void $ W.registerStatusNotifierItem client busName

  -- Watch kanshi config for changes
  configDir <- getXdgDirectory XdgConfig "kanshi"
  dirExists <- doesDirectoryExist configDir
  when dirExists $
    void $ forkIO $ withManager $ \mgr -> do
      void $ watchDir mgr configDir (const True) $ \event ->
        case event of
          Modified {} -> refreshState sniState
          Added {} -> refreshState sniState
          _ -> pure ()
      forever $ threadDelay maxBound

  -- Block forever on main thread
  forever $ threadDelay maxBound

exportSNI :: Client -> String -> String -> String -> IO ()
exportSNI client _busName path menuPath = do
  let clientInterface =
        Interface
          { interfaceName = interfaceName_ "org.kde.StatusNotifierItem"
          , interfaceMethods = []
          , interfaceProperties =
              [ readOnlyProperty "Category" (pure ("Hardware" :: String))
              , readOnlyProperty "Id" (pure ("kanshi-sni" :: String))
              , readOnlyProperty "Title" (pure ("Display Profiles" :: String))
              , readOnlyProperty "Status" (pure ("Active" :: String))
              , readOnlyProperty "WindowId" (pure (0 :: Int32))
              , readOnlyProperty "IconName" (pure ("video-display" :: String))
              , readOnlyProperty "ItemIsMenu" (pure True)
              , readOnlyProperty "Menu" (pure (objectPath_ menuPath))
              ]
          , interfaceSignals = []
          }
  export client (fromString path) clientInterface

tryConnect :: IO (Maybe KanshiConnection)
tryConnect =
  connectKanshi >>= \case
    Left _ -> pure Nothing
    Right conn -> pure (Just conn)

buildInitialState :: Maybe KanshiConnection -> IO AppState
buildInitialState mConn = do
  profileSpecs <- either (const []) id <$> getProfiles
  let profiles = map profileSpecName profileSpecs
  (curProfile, pendingProfile) <- case mConn of
    Nothing -> pure (Nothing, Nothing)
    Just conn -> do
      result <- kanshiStatus conn
      pure $ case result of
        Right status -> (currentProfile status, pendingProfile status)
        Left _ -> (Nothing, Nothing)
  monitors <- either (const []) id <$> getMonitors
  pure
    AppState
      { stateProfiles = profiles
      , stateCurrentProfile = curProfile
      , statePendingProfile = pendingProfile
      , stateMonitors = monitors
      , stateProfileSpecs = profileSpecs
      , stateKanshiConnected = case mConn of
          Nothing -> False
          Just _ -> True
      }

rebuildMenu :: SNIState -> IO ()
rebuildMenu sniState = do
  state <- readMVar (sniAppState sniState)
  let actions =
        MenuActions
          { onSwitchProfile = handleSwitchProfile sniState
          , onReloadConfig = handleReload sniState
          , onSetMode = handleSetMode sniState
          , onSetScale = handleSetScale sniState
          , onToggleMonitor = handleToggleMonitor sniState
          }
  newRoot <- buildMenu state actions
  runOnGLibMain (sniGLibContext sniState) $
    Dbusmenu.serverSetRoot (sniMenuServer sniState) newRoot

refreshState :: SNIState -> IO ()
refreshState sniState = do
  mConn <- readMVar (sniConnection sniState)
  newState <- buildInitialState mConn
  modifyMVar_ (sniAppState sniState) $ const (pure newState)
  rebuildMenu sniState

-- Schedule work on the GLib main context.
runOnGLibMain :: GLib.MainContext -> IO () -> IO ()
runOnGLibMain context action = do
  sp <- newStablePtr action
  GI.withManagedPtr context $ \ctxPtr ->
    c_g_main_context_invoke_full ctxPtr 4 invokeSourceFunc (castStablePtrToPtr sp) destroyNotifyFunc

type InvokeSourceFunc = Ptr () -> IO CInt
type DestroyNotify = Ptr () -> IO ()

foreign import ccall unsafe "g_main_context_invoke_full"
  c_g_main_context_invoke_full
    :: Ptr GLib.MainContext
    -> CInt
    -> FunPtr InvokeSourceFunc
    -> Ptr ()
    -> FunPtr DestroyNotify
    -> IO ()

foreign import ccall "wrapper"
  mkInvokeSourceFunc :: InvokeSourceFunc -> IO (FunPtr InvokeSourceFunc)

foreign import ccall "wrapper"
  mkDestroyNotify :: DestroyNotify -> IO (FunPtr DestroyNotify)

{-# NOINLINE invokeSourceFunc #-}
invokeSourceFunc :: FunPtr InvokeSourceFunc
invokeSourceFunc = unsafePerformIO $
  mkInvokeSourceFunc $ \p -> do
    let sp :: StablePtr (IO ())
        sp = castPtrToStablePtr p
    (deRefStablePtr sp >>= \io -> io) `catch` \(_ :: SomeException) -> pure ()
    pure 0

{-# NOINLINE destroyNotifyFunc #-}
destroyNotifyFunc :: FunPtr DestroyNotify
destroyNotifyFunc = unsafePerformIO $
  mkDestroyNotify $ \p -> do
    let sp :: StablePtr (IO ())
        sp = castPtrToStablePtr p
    freeStablePtr sp

withKanshiConn :: SNIState -> (KanshiConnection -> IO (Either KanshiError a)) -> IO ()
withKanshiConn sniState action = do
  mConn <- readMVar (sniConnection sniState)
  case mConn of
    Nothing -> pure ()
    Just conn -> do
      result <- action conn
      case result of
        Left _ -> resetConnection sniState
        Right _ -> pure ()

resetConnection :: SNIState -> IO ()
resetConnection sniState = do
  mConn <- takeMVar (sniConnection sniState)
  case mConn of
    Just conn -> disconnectKanshi conn
    Nothing -> pure ()
  newConn <- tryConnect
  putMVar (sniConnection sniState) newConn

handleSwitchProfile :: SNIState -> Text -> IO ()
handleSwitchProfile sniState profile = do
  withKanshiConn sniState $ \conn -> kanshiSwitch conn profile
  refreshState sniState

handleReload :: SNIState -> IO ()
handleReload sniState = do
  mConn <- readMVar (sniConnection sniState)
  case mConn of
    Nothing -> do
      newConn <- tryConnect
      modifyMVar_ (sniConnection sniState) $ const (pure newConn)
    Just conn -> do
      result <- kanshiReload conn
      case result of
        Left _ -> resetConnection sniState
        Right _ -> pure ()
  refreshState sniState

handleSetMode :: SNIState -> Text -> Text -> IO ()
handleSetMode sniState name mode = do
  void $ setMonitorMode name mode
  refreshState sniState

handleSetScale :: SNIState -> Text -> Double -> IO ()
handleSetScale sniState name scale = do
  void $ Hyprland.Monitors.setMonitorScale name scale
  refreshState sniState

handleToggleMonitor :: SNIState -> Text -> Bool -> IO ()
handleToggleMonitor sniState name currentlyEnabled = do
  void $
    if currentlyEnabled
      then disableMonitor name
      else enableMonitor name
  refreshState sniState
