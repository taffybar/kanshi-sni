# kanshi-sni Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Build a standalone Haskell SNI tray application for switching kanshi display profiles and adjusting Hyprland monitor settings.

**Architecture:** A single-binary SNI app that registers on DBus with a DBusMenu. It speaks Varlink to kanshi for profile management, shells out to hyprctl for monitor queries/commands, and parses the kanshi config file for profile names. Uses gi-dbusmenu for the menu, status-notifier-item for SNI registration.

**Tech Stack:** Haskell, status-notifier-item, gi-dbusmenu, gi-gio, gi-glib, dbus, aeson, megaparsec, network, process, fsnotify

---

### Task 1: Project Skeleton

**Files:**
- Create: `kanshi-sni.cabal`
- Create: `cabal.project` (if needed for local deps)
- Create: `app/Main.hs`
- Create: `flake.nix` (nix build support)

**Step 1: Create the cabal file**

```cabal
cabal-version: 2.4
name:           kanshi-sni
version:        0.1.0.0
synopsis:       SNI tray icon for kanshi display profile management
license:        BSD-3-Clause
author:         Ivan Malison
maintainer:     IvanMalison@gmail.com
build-type:     Simple

library
  exposed-modules:
      Kanshi.Config
      Kanshi.Varlink
      Kanshi.SNI
      Hyprland.Monitors
      Menu
  hs-source-dirs: src
  default-extensions:
      OverloadedStrings
      RecordWildCards
      ScopedTypeVariables
      LambdaCase
  build-depends:
      base >= 4.7 && < 5
    , aeson
    , bytestring
    , containers
    , dbus >= 1.2.1 && < 2.0.0
    , directory
    , filepath
    , fsnotify
    , gi-dbusmenu
    , gi-gio
    , gi-glib
    , hslogger
    , megaparsec
    , network
    , process
    , status-notifier-item >= 0.3.0.0
    , text
    , transformers
  default-language: Haskell2010

executable kanshi-sni
  main-is: Main.hs
  hs-source-dirs: app
  default-extensions:
      OverloadedStrings
      RecordWildCards
  ghc-options: -threaded -rtsopts -with-rtsopts=-N
  build-depends:
      base >= 4.7 && < 5
    , kanshi-sni
    , hslogger
  default-language: Haskell2010
```

**Step 2: Create stub Main.hs**

```haskell
module Main where

main :: IO ()
main = putStrLn "kanshi-sni"
```

**Step 3: Create stub library modules**

Create empty module files for each exposed module with just the module header.

**Step 4: Initialize git repo**

```bash
cd ~/Projects/kanshi-sni
git init
git add .
git commit -m "feat: initial project skeleton"
```

**Step 5: Verify it builds**

```bash
cd ~/Projects/kanshi-sni
cabal build
```

---

### Task 2: Kanshi Config Parser

**Files:**
- Create: `src/Kanshi/Config.hs`
- Create: `test/Kanshi/ConfigSpec.hs` (if adding test suite)

**Step 1: Write the config parser**

Parse kanshi config to extract profile names. The format supports:
- `profile <name> { ... }` blocks (name can be quoted or unquoted)
- `# comments`
- `include` directives (ignore for now)
- Anonymous profiles (bare `{ ... }` blocks - skip these)

```haskell
module Kanshi.Config
  ( parseKanshiConfig
  , getProfileNames
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Void
import System.Directory (getXdgDirectory, XdgDirectory(..))
import System.FilePath ((</>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- Skip whitespace and comments
sc :: Parser ()
sc = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- Parse a quoted or unquoted name
profileName :: Parser Text
profileName = quotedName <|> unquotedName
  where
    quotedName = do
      q <- char '"' <|> char '\''
      name <- takeWhileP Nothing (/= q)
      void $ char q
      return name
    unquotedName =
      takeWhile1P (Just "profile name") (\c -> c /= '{' && c /= '}' && c /= ' ' && c /= '\t' && c /= '\n' && c /= '\r')

-- Skip everything inside braces (handles nesting)
bracedBlock :: Parser ()
bracedBlock = do
  void $ lexeme $ char '{'
  skipManyTill (bracedBlock <|> void (satisfy (/= '}'))) (char '}')
  sc

-- Parse a profile declaration, return its name
profileDecl :: Parser (Maybe Text)
profileDecl = do
  void $ lexeme $ string "profile"
  name <- lexeme profileName
  bracedBlock
  return $ Just name

-- Skip any other top-level block (output defaults, include, anonymous blocks)
otherDecl :: Parser (Maybe Text)
otherDecl = do
  void $ lexeme $ choice
    [ string "output" >> skipManyTill anySingle (char '{') >> void (bracedBlock' )
    , string "include" >> void (takeWhileP Nothing (/= '\n'))
    , void bracedBlock -- anonymous profile
    ]
  return Nothing
  where
    bracedBlock' = skipManyTill (void bracedBlock <|> void (satisfy (/= '}'))) (char '}') >> sc

-- Top-level parser
kanshiConfig :: Parser [Text]
kanshiConfig = do
  sc
  results <- many (profileDecl <|> otherDecl)
  eof
  return [name | Just name <- results]

parseKanshiConfig :: Text -> Either String [Text]
parseKanshiConfig input =
  case parse kanshiConfig "<kanshi config>" input of
    Left err -> Left $ errorBundlePretty err
    Right names -> Right names

-- | Read and parse the default kanshi config file
getProfileNames :: IO (Either String [Text])
getProfileNames = do
  configDir <- getXdgDirectory XdgConfig "kanshi"
  let configPath = configDir </> "config"
  result <- try' $ TIO.readFile configPath
  case result of
    Left err -> return $ Left $ "Could not read config: " ++ show err
    Right content -> return $ parseKanshiConfig content
  where
    try' :: IO a -> IO (Either IOError a)
    try' action = (Right <$> action) `catch` (return . Left)
```

Note: The parser above is a starting sketch. It may need refinement for edge cases during implementation. The key thing is extracting profile names from `profile <name> { ... }` blocks.

**Step 2: Test manually**

Create a sample kanshi config at `~/.config/kanshi/config`:
```
# Test config
profile laptop {
    output eDP-1 enable mode 2560x1600@240Hz scale 1.0
}

profile docked {
    output eDP-1 disable
    output DP-3 enable mode 3840x2160 scale 1.5
}
```

Build and test in ghci:
```bash
cabal repl
> import Kanshi.Config
> getProfileNames
Right ["laptop","docked"]
```

**Step 3: Commit**

```bash
git add src/Kanshi/Config.hs
git commit -m "feat: kanshi config parser for profile names"
```

---

### Task 3: Kanshi Varlink Client

**Files:**
- Create: `src/Kanshi/Varlink.hs`

**Step 1: Implement the Varlink client**

Varlink protocol: send JSON + `\0` over Unix socket, receive JSON + `\0` back.

```haskell
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

import Control.Exception (try, IOException, bracket)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Environment (lookupEnv)
import System.Posix.User (getEffectiveUserID)
import Text.Printf (printf)

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

-- | Discover the kanshi Varlink socket path
getSocketPath :: IO (Either KanshiError FilePath)
getSocketPath = do
  uid <- getEffectiveUserID
  mDisplay <- lookupEnv "WAYLAND_DISPLAY"
  case mDisplay of
    Nothing -> return $ Left $ ConnectionFailed "WAYLAND_DISPLAY not set"
    Just display ->
      return $ Right $ printf "/run/user/%d/fr.emersion.kanshi.%s" uid display

-- | Connect to kanshi's Varlink socket
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
      case result of
        Left (e :: IOException) ->
          return $ Left $ ConnectionFailed $ "Could not connect: " ++ show e
        Right sock ->
          return $ Right $ KanshiConnection sock

disconnectKanshi :: KanshiConnection -> IO ()
disconnectKanshi (KanshiConnection sock) = close sock

-- | Send a Varlink method call and receive the response
varlinkCall :: KanshiConnection -> Value -> IO (Either KanshiError Value)
varlinkCall (KanshiConnection sock) request = do
  sendAll sock $ LBS.toStrict (encode request) <> BS.singleton 0
  response <- recvUntilNull sock
  case eitherDecodeStrict response of
    Left err -> return $ Left $ ProtocolError $ "JSON decode error: " ++ err
    Right val -> return $ parseVarlinkResponse val

-- | Read from socket until we hit a NUL byte
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

-- | Parse a Varlink response, checking for errors
parseVarlinkResponse :: Value -> Either KanshiError Value
parseVarlinkResponse (Object obj)
  | Just (String errName) <- KM.lookup "error" obj =
      Left $ case errName of
        "fr.emersion.kanshi.ProfileNotFound" -> ProfileNotFound
        "fr.emersion.kanshi.ProfileNotMatched" -> ProfileNotMatched
        "fr.emersion.kanshi.ProfileNotApplied" -> ProfileNotApplied
        other -> ProtocolError $ "Varlink error: " ++ T.unpack other
  | otherwise = Right (Object obj)
parseVarlinkResponse v = Right v

-- | Query kanshi status
kanshiStatus :: KanshiConnection -> IO (Either KanshiError KanshiStatus)
kanshiStatus conn = do
  result <- varlinkCall conn $ object
    [ "method" .= ("fr.emersion.kanshi.Status" :: Text) ]
  return $ case result of
    Left err -> Left err
    Right (Object obj) ->
      let params = case KM.lookup "parameters" obj of
            Just (Object p) -> p
            _ -> KM.empty
          getCurrent = case KM.lookup "current_profile" params of
            Just (String s) -> Just s
            _ -> Nothing
          getPending = case KM.lookup "pending_profile" params of
            Just (String s) -> Just s
            _ -> Nothing
      in Right $ KanshiStatus getCurrent getPending
    Right _ -> Left $ ProtocolError "Unexpected response format"

-- | Switch to a named profile
kanshiSwitch :: KanshiConnection -> Text -> IO (Either KanshiError ())
kanshiSwitch conn profile = do
  result <- varlinkCall conn $ object
    [ "method" .= ("fr.emersion.kanshi.Switch" :: Text)
    , "parameters" .= object [ "profile" .= profile ]
    ]
  return $ case result of
    Left err -> Left err
    Right _ -> Right ()

-- | Reload kanshi configuration
kanshiReload :: KanshiConnection -> IO (Either KanshiError ())
kanshiReload conn = do
  result <- varlinkCall conn $ object
    [ "method" .= ("fr.emersion.kanshi.Reload" :: Text) ]
  return $ case result of
    Left err -> Left err
    Right _ -> Right ()
```

**Step 2: Test in ghci** (requires kanshi running with a config)

```bash
cabal repl
> import Kanshi.Varlink
> Right conn <- connectKanshi
> kanshiStatus conn
Right (KanshiStatus {currentProfile = Just "laptop", pendingProfile = Nothing})
> disconnectKanshi conn
```

**Step 3: Commit**

```bash
git add src/Kanshi/Varlink.hs
git commit -m "feat: kanshi Varlink IPC client"
```

---

### Task 4: Hyprland Monitor Queries & Commands

**Files:**
- Create: `src/Hyprland/Monitors.hs`

**Step 1: Implement monitor types and queries**

```haskell
module Hyprland.Monitors
  ( MonitorInfo(..)
  , getMonitors
  , setMonitorMode
  , setMonitorScale
  , disableMonitor
  , enableMonitor
  ) where

import Data.Aeson
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

getMonitors :: IO (Either String [MonitorInfo])
getMonitors = do
  output <- readProcess "hyprctl" ["monitors", "-j"] ""
  return $ eitherDecode (fromString output)
  where fromString = Data.Aeson.encode -- will actually use LBS

-- | Set monitor to a specific mode string like "2560x1600@240Hz"
setMonitorMode :: Text -> Text -> IO ()
setMonitorMode name mode =
  callProcess "hyprctl" ["keyword", "monitor", T.unpack $ name <> "," <> mode <> ",auto,1"]

-- | Set monitor scale
setMonitorScale :: Text -> Double -> IO ()
setMonitorScale name scale =
  callProcess "hyprctl" ["keyword", "monitor",
    T.unpack $ name <> ",preferred,auto," <> T.pack (show scale)]

-- | Disable a monitor
disableMonitor :: Text -> IO ()
disableMonitor name =
  callProcess "hyprctl" ["keyword", "monitor", T.unpack $ name <> ",disabled"]

-- | Re-enable a monitor with preferred settings
enableMonitor :: Text -> IO ()
enableMonitor name =
  callProcess "hyprctl" ["keyword", "monitor", T.unpack $ name <> ",preferred,auto,1"]
```

Note: The `getMonitors` JSON decoding needs `Data.ByteString.Lazy.Char8.pack` rather than re-encoding. This will be corrected during implementation.

**Step 2: Test in ghci**

```bash
cabal repl
> import Hyprland.Monitors
> getMonitors
Right [MonitorInfo {monitorName = "eDP-1", ...}]
```

**Step 3: Commit**

```bash
git add src/Hyprland/Monitors.hs
git commit -m "feat: hyprland monitor queries and commands"
```

---

### Task 5: Menu Builder

**Files:**
- Create: `src/Menu.hs`

**Step 1: Implement the menu builder**

This module takes an `AppState` and a set of action callbacks, and builds a `Menuitem` tree using gi-dbusmenu.

```haskell
module Menu
  ( AppState(..)
  , MenuActions(..)
  , buildMenu
  ) where

import Control.Monad (forM_, when)
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import GI.Dbusmenu
import Hyprland.Monitors (MonitorInfo(..))

data AppState = AppState
  { stateProfiles :: [Text]
  , stateCurrentProfile :: Maybe Text
  , stateMonitors :: [MonitorInfo]
  , stateKanshiConnected :: Bool
  } deriving (Show)

data MenuActions = MenuActions
  { onSwitchProfile :: Text -> IO ()
  , onReloadConfig :: IO ()
  , onSetMode :: Text -> Text -> IO ()     -- monitor name, mode
  , onSetScale :: Text -> Double -> IO ()   -- monitor name, scale
  , onToggleMonitor :: Text -> Bool -> IO () -- monitor name, currently enabled?
  }

-- | Build the full menu tree from current state
buildMenu :: AppState -> MenuActions -> IO Menuitem
buildMenu state actions = do
  root <- menuitemNew

  if stateKanshiConnected state
    then do
      -- Header: current profile
      header <- makeLabel $ case stateCurrentProfile state of
        Just p -> "Profile: " <> p
        Nothing -> "No active profile"
      menuitemPropertySet header "enabled" "false"
      menuitemChildAppend root header

      addSeparator root

      -- Profile list
      forM_ (stateProfiles state) $ \profile -> do
        item <- makeLabel profile
        when (stateCurrentProfile state == Just profile) $
          menuitemPropertySet item "toggle-state" "1"
        menuitemPropertySet item "toggle-type" "radio"
        _ <- onMenuitemItemActivated item $ \_ ->
          onSwitchProfile actions profile
        menuitemChildAppend root item

      addSeparator root

      -- Reload config
      reloadItem <- makeLabel "Reload Config"
      _ <- onMenuitemItemActivated reloadItem $ \_ ->
        onReloadConfig actions
      menuitemChildAppend root reloadItem

    else do
      disconnected <- makeLabel "kanshi not connected"
      menuitemPropertySet disconnected "enabled" "false"
      menuitemChildAppend root disconnected

      retryItem <- makeLabel "Retry Connection"
      _ <- onMenuitemItemActivated retryItem $ \_ ->
        onReloadConfig actions
      menuitemChildAppend root retryItem

  -- Monitor submenus (always shown if hyprland available)
  when (not $ null $ stateMonitors state) $ do
    addSeparator root
    forM_ (stateMonitors state) $ \mon -> do
      monitorSubmenu <- buildMonitorSubmenu mon actions
      menuitemChildAppend root monitorSubmenu

  return root

buildMonitorSubmenu :: MonitorInfo -> MenuActions -> IO Menuitem
buildMonitorSubmenu mon actions = do
  let label = monitorName mon <> " (" <>
              T.pack (show (monitorWidth mon)) <> "x" <>
              T.pack (show (monitorHeight mon)) <> " @ " <>
              T.pack (show (monitorScale mon)) <> "x)"
  parent <- makeLabel label
  submenu <- menuitemNew

  -- Enable/Disable toggle
  toggle <- makeLabel $ if monitorDisabled mon then "Enable" else "Disable"
  _ <- onMenuitemItemActivated toggle $ \_ ->
    onToggleMonitor actions (monitorName mon) (not $ monitorDisabled mon)
  menuitemChildAppend submenu toggle

  -- Resolution submenu
  resParent <- makeLabel "Resolution"
  resSub <- menuitemNew
  forM_ (monitorAvailableModes mon) $ \mode -> do
    modeItem <- makeLabel mode
    let currentMode = T.pack (show (monitorWidth mon)) <> "x" <>
                      T.pack (show (monitorHeight mon)) <> "@" <>
                      T.pack (show (monitorRefreshRate mon)) <> "Hz"
    -- Approximate match (hyprctl formats differ slightly)
    menuitemPropertySet modeItem "toggle-type" "radio"
    _ <- onMenuitemItemActivated modeItem $ \_ ->
      onSetMode actions (monitorName mon) mode
    menuitemChildAppend resSub modeItem
  menuitemPropertySet resParent "children-display" "submenu"
  menuitemChildAppend submenu resParent

  -- Scale submenu
  scaleParent <- makeLabel "Scale"
  scaleSub <- menuitemNew
  forM_ [1.0, 1.25, 1.5, 1.75, 2.0 :: Double] $ \s -> do
    scaleItem <- makeLabel $ T.pack $ show s
    menuitemPropertySet scaleItem "toggle-type" "radio"
    when (abs (monitorScale mon - s) < 0.01) $
      menuitemPropertySet scaleItem "toggle-state" "1"
    _ <- onMenuitemItemActivated scaleItem $ \_ ->
      onSetScale actions (monitorName mon) s
    menuitemChildAppend scaleSub scaleItem
  menuitemPropertySet scaleParent "children-display" "submenu"
  menuitemChildAppend submenu scaleParent

  menuitemPropertySet parent "children-display" "submenu"
  return parent

-- Helpers

makeLabel :: Text -> IO Menuitem
makeLabel text = do
  item <- menuitemNew
  variant <- toGVariant text
  menuitemPropertySetVariant item "label" variant
  return item

addSeparator :: Menuitem -> IO ()
addSeparator parent = do
  sep <- menuitemNew
  menuitemPropertySet sep "type" "separator"
  menuitemChildAppend parent sep
```

Note: The gi-dbusmenu API for submenus and signal connections will need verification during implementation - the child-append vs submenu relationship may need `menuitemChildAppend` on the submenu root differently. The reference code from notifications-tray-icon uses `serverSetRoot` to swap the whole tree.

**Step 2: Commit**

```bash
git add src/Menu.hs
git commit -m "feat: menu builder for kanshi profiles and monitor settings"
```

---

### Task 6: SNI Registration & Wiring

**Files:**
- Create: `src/Kanshi/SNI.hs`

**Step 1: Implement SNI registration with menu server**

Follow the pattern from `notifications-tray-icon/OverlayIcon.hs`:

```haskell
module Kanshi.SNI
  ( startSNI
  ) where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad (void)
import Data.IORef
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import DBus
import DBus.Client
import GI.Dbusmenu
import qualified GI.GLib as GLib
import qualified GI.Gio as Gio
import qualified GI.Gio.Objects.Cancellable as Gio
import qualified StatusNotifier.Watcher.Client as W

import Hyprland.Monitors
import Kanshi.Config
import Kanshi.Varlink
import Menu

data SNIState = SNIState
  { sniConnection :: MVar (Maybe KanshiConnection)
  , sniAppState :: MVar AppState
  , sniMenuServer :: Server
  , sniGLibContext :: GLib.MainContext
  , sniDBusClient :: Client
  }

startSNI :: IO ()
startSNI = do
  let busName = "org.kanshi.SNI"
      path = "/StatusNotifierItem"
      menuPath = "/StatusNotifierItem/Menu"
      menuBusName = busName ++ ".Menu"

  -- DBus setup
  client <- connectSession

  -- GLib main loop (needed for gi-dbusmenu)
  mainLoop <- GLib.mainLoopNew Nothing False >>= GLib.mainLoopRef
  context <- GLib.mainLoopGetContext mainLoop

  -- Menu server
  connection <- Just <$> Gio.cancellableNew >>= Gio.busGetSync Gio.BusTypeSession
  Gio.busOwnNameOnConnection connection (T.pack menuBusName) [] Nothing Nothing
  menuServer <- serverNew (T.pack menuPath)

  -- State
  kanshiConn <- tryConnect
  appState <- buildInitialState kanshiConn
  connVar <- newMVar kanshiConn
  stateVar <- newMVar appState

  let sniState = SNIState connVar stateVar menuServer context client

  -- Build initial menu and set it
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

  -- TODO: Set up fsnotify watcher for kanshi config

  -- Run GLib main loop (blocks)
  forkIO $ GLib.mainLoopRun mainLoop

  -- Hang forever
  void $ forever $ threadDelay maxBound

tryConnect :: IO (Maybe KanshiConnection)
tryConnect = do
  result <- connectKanshi
  case result of
    Left _ -> return Nothing
    Right conn -> return $ Just conn

buildInitialState :: Maybe KanshiConnection -> IO AppState
buildInitialState mConn = do
  profiles <- either (const []) id <$> getProfileNames
  currentProfile <- case mConn of
    Nothing -> return Nothing
    Just conn -> do
      result <- kanshiStatus conn
      return $ case result of
        Right status -> Kanshi.Varlink.currentProfile status
        Left _ -> Nothing
  monitors <- either (const []) id <$> getMonitors
  return AppState
    { stateProfiles = profiles
    , stateCurrentProfile = currentProfile
    , stateMonitors = monitors
    , stateKanshiConnected = mConn /= Nothing
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
  let runOnMain action =
        GLib.mainContextInvokeFull (Just $ sniGLibContext sniState) 4 $
          action >> return False
  void $ runOnMain $ serverSetRoot (sniMenuServer sniState) newRoot

refreshState :: SNIState -> IO ()
refreshState sniState = do
  mConn <- readMVar (sniConnection sniState)
  newState <- buildInitialState mConn
  modifyMVar_ (sniAppState sniState) $ const $ return newState
  rebuildMenu sniState

handleSwitchProfile :: SNIState -> Text -> IO ()
handleSwitchProfile sniState profile = do
  mConn <- readMVar (sniConnection sniState)
  case mConn of
    Nothing -> return ()
    Just conn -> do
      void $ kanshiSwitch conn profile
      refreshState sniState

handleReload :: SNIState -> IO ()
handleReload sniState = do
  mConn <- readMVar (sniConnection sniState)
  case mConn of
    Nothing -> do
      -- Try to reconnect
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
```

**Step 2: Commit**

```bash
git add src/Kanshi/SNI.hs
git commit -m "feat: SNI registration and menu wiring"
```

---

### Task 7: Wire Up Main.hs & End-to-End Test

**Files:**
- Modify: `app/Main.hs`

**Step 1: Wire up main**

```haskell
module Main where

import Kanshi.SNI (startSNI)
import System.Log.Logger

main :: IO ()
main = do
  updateGlobalLogger rootLoggerName (setLevel WARNING)
  startSNI
```

**Step 2: Build and run**

```bash
cd ~/Projects/kanshi-sni
cabal build
```

**Step 3: Create a test kanshi config**

```bash
mkdir -p ~/.config/kanshi
cat > ~/.config/kanshi/config << 'EOF'
profile laptop {
    output eDP-1 enable mode 2560x1600@240Hz scale 1.0
}

profile docked {
    output eDP-1 disable
    output DP-3 enable mode 3840x2160 scale 1.5
}
EOF
```

**Step 4: Start kanshi and test**

```bash
# Start kanshi if not running
kanshi &

# Run kanshi-sni (requires status-notifier-watcher running)
cabal run kanshi-sni
```

Verify: the SNI icon appears in taffybar's tray, clicking shows profiles and monitors.

**Step 5: Commit**

```bash
git add app/Main.hs
git commit -m "feat: wire up main entry point"
```

---

### Task 8: File Watcher for Config Changes

**Files:**
- Modify: `src/Kanshi/SNI.hs`

**Step 1: Add fsnotify watcher**

In `startSNI`, after registering the SNI, add a file watcher on the kanshi config:

```haskell
import System.FSNotify (withManager, watchDir, Event(..))
import System.Directory (getXdgDirectory, XdgDirectory(..))

-- In startSNI, before the main loop:
configDir <- getXdgDirectory XdgConfig "kanshi"
void $ forkIO $ withManager $ \mgr -> do
  void $ watchDir mgr configDir (const True) $ \event ->
    case event of
      Modified {} -> refreshState sniState
      _ -> return ()
  forever $ threadDelay maxBound
```

**Step 2: Test**

Edit `~/.config/kanshi/config`, add a new profile. The menu should update automatically.

**Step 3: Commit**

```bash
git add src/Kanshi/SNI.hs
git commit -m "feat: watch kanshi config for changes and rebuild menu"
```

---

### Task 9: Nix Flake & Packaging

**Files:**
- Create: `flake.nix`
- Create: `flake.lock` (auto-generated)

**Step 1: Create flake.nix**

Model after the taffybar or notifications-tray-icon flake if one exists. Basic structure:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        kanshi-sni = haskellPackages.callCabal2nix "kanshi-sni" ./. {};
      in {
        packages.default = kanshi-sni;
        devShells.default = haskellPackages.shellFor {
          packages = p: [ kanshi-sni ];
          buildInputs = with haskellPackages; [
            cabal-install
            ghcid
          ];
        };
      });
}
```

**Step 2: Test nix build**

```bash
cd ~/Projects/kanshi-sni
nix build
```

**Step 3: Commit**

```bash
git add flake.nix flake.lock
git commit -m "feat: nix flake for building and development"
```

---

### Task 10: Add to Hyprland Session Startup

**Files:**
- Modify: NixOS/Hyprland configuration (user's dotfiles)

**Step 1: Add kanshi-sni to session startup**

This depends on how the user starts their tray apps. Likely add to Hyprland exec-once or systemd user service. Determine the appropriate place and add it.

**Step 2: Test full flow**

1. Log out and back in (or restart Hyprland)
2. Verify kanshi-sni appears in tray
3. Click and verify profiles show up
4. Switch a profile and verify it works
5. Check monitor submenus show correct data
6. Change a resolution/scale and verify

**Step 3: Commit**

```bash
git add <modified-config-files>
git commit -m "feat: add kanshi-sni to session startup"
```
