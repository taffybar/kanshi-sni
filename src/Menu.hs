module Menu
  ( AppState(..)
  , MenuActions(..)
  , buildMenu
  ) where

import Control.Monad (forM_, when, void)
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import GI.Dbusmenu
import Hyprland.Monitors (MonitorInfo(..))
import Kanshi.Config (ProfileSpec(..), OutputSpec(..))

data AppState = AppState
  { stateProfiles :: [Text]
  , stateCurrentProfile :: Maybe Text
  , statePendingProfile :: Maybe Text
  , stateMonitors :: [MonitorInfo]
  , stateProfileSpecs :: [ProfileSpec]
  , stateKanshiConnected :: Bool
  } deriving (Show)

data MenuActions = MenuActions
  { onSwitchProfile :: Text -> IO ()
  , onReloadConfig :: IO ()
  , onSetMode :: Text -> Text -> IO ()
  , onSetScale :: Text -> Double -> IO ()
  , onToggleMonitor :: Text -> Bool -> IO ()
  }

-- | Build a complete DBusMenu tree from the current application state.
buildMenu :: AppState -> MenuActions -> IO Menuitem
buildMenu state actions = do
  root <- menuitemNew

  -- Header: current profile / status
  let headerText =
        if stateKanshiConnected state
          then case (stateCurrentProfile state, statePendingProfile state) of
            (Just p, Just pending) | pending /= p ->
              "Profile: " <> p <> " (pending: " <> pending <> ")"
            (Just p, _) -> "Profile: " <> p
            (Nothing, Just pending) -> "No active profile (pending: " <> pending <> ")"
            (Nothing, Nothing) -> "No active profile"
          else "kanshi not connected"
  header <- makeLabel headerText
  setEnabled header False
  menuitemChildAppend root header

  monitorsHeader <- makeLabel $ "Monitors: " <> T.pack (show (length (stateMonitors state)))
  setEnabled monitorsHeader False
  menuitemChildAppend root monitorsHeader

  addSeparator root

  -- Current monitor details (read-only)
  when (not $ null $ stateMonitors state) $ do
    currentSub <- makeLabel "Current Setup"
    menuitemPropertySet currentSub "children-display" "submenu"
    forM_ (stateMonitors state) $ \mon -> do
      let monLabel =
            monitorName mon <> ": " <>
            T.pack (show (monitorWidth mon)) <> "x" <>
            T.pack (show (monitorHeight mon)) <> "@" <>
            T.pack (showFF (monitorRefreshRate mon)) <> "Hz" <>
            " scale " <> T.pack (show (monitorScale mon)) <>
            " pos " <> T.pack (show (monitorX mon)) <> "," <> T.pack (show (monitorY mon)) <>
            (if monitorDisabled mon then " (disabled)" else "") <>
            (if monitorFocused mon then " (focused)" else "")
      item <- makeLabel monLabel
      setEnabled item False
      menuitemChildAppend currentSub item
    void $ menuitemChildAppend root currentSub

  -- Profile list (radio items) only makes sense when kanshi is connected.
  when (stateKanshiConnected state) $ do
    forM_ (stateProfiles state) $ \profile -> do
      let outCount =
            case filter (\p -> profileSpecName p == profile) (stateProfileSpecs state) of
              (p:_) -> length (profileSpecOutputs p)
              [] -> 0
          label =
            if outCount > 0
              then profile <> " (" <> T.pack (show outCount) <> ")"
              else profile
      item <- makeLabel label
      setToggleType item "radio"
      if stateCurrentProfile state == Just profile
        then setToggleState item 1
        else setToggleState item 0
      void $ onMenuitemItemActivated item $ \_ ->
        onSwitchProfile actions profile
      menuitemChildAppend root item

  -- Profile details always shown (useful even when kanshi isn't running).
  when (not $ null $ stateProfileSpecs state) $ do
    details <- makeLabel "Profile Details"
    menuitemPropertySet details "children-display" "submenu"
    forM_ (stateProfileSpecs state) $ \spec -> do
      specItem <- makeLabel $ profileSpecName spec <> " (" <> T.pack (show (length (profileSpecOutputs spec))) <> ")"
      menuitemPropertySet specItem "children-display" "submenu"
      activate <- makeLabel "Activate"
      if stateKanshiConnected state
        then void $ onMenuitemItemActivated activate $ \_ ->
          onSwitchProfile actions (profileSpecName spec)
        else setEnabled activate False
      menuitemChildAppend specItem activate
      forM_ (profileSpecOutputs spec) $ \out -> do
        let outLabel =
              outputTarget out <>
              maybe "" (\b -> if b then " enable" else " disable") (outputEnabled out) <>
              maybe "" (\m -> " mode " <> m) (outputMode out) <>
              maybe "" (\(x,y) -> " pos " <> T.pack (show x) <> "," <> T.pack (show y)) (outputPosition out) <>
              maybe "" (\s -> " scale " <> T.pack (show s)) (outputScale out)
        outItem <- makeLabel outLabel
        setEnabled outItem False
        menuitemChildAppend specItem outItem
      menuitemChildAppend details specItem
    void $ menuitemChildAppend root details

  addSeparator root

  -- Reload config / retry connection
  reloadItem <- makeLabel $ if stateKanshiConnected state then "Reload Config" else "Retry Connection"
  void $ onMenuitemItemActivated reloadItem $ \_ ->
    onReloadConfig actions
  menuitemChildAppend root reloadItem

  -- Monitor submenus (always shown when monitors are available)
  when (not $ null $ stateMonitors state) $ do
    addSeparator root
    forM_ (stateMonitors state) $ \mon -> do
      monitorSubmenu <- buildMonitorSubmenu mon actions
      menuitemChildAppend root monitorSubmenu

  return root

-- | Build a submenu for a single monitor with resolution, scale, and
-- enable/disable controls.
buildMonitorSubmenu :: MonitorInfo -> MenuActions -> IO Menuitem
buildMonitorSubmenu mon actions = do
  let label =
        monitorName mon <> " (" <>
        T.pack (show (monitorWidth mon)) <> "x" <>
        T.pack (show (monitorHeight mon)) <> "@" <>
        T.pack (showFF (monitorRefreshRate mon)) <> "Hz" <>
        " scale " <> T.pack (show (monitorScale mon)) <>
        " pos " <> T.pack (show (monitorX mon)) <> "," <> T.pack (show (monitorY mon)) <>
        (if monitorDisabled mon then " disabled" else "") <>
        ")"
  parent <- makeLabel label
  menuitemPropertySet parent "children-display" "submenu"

  -- Enable/Disable toggle
  toggle <- makeLabel $ if monitorDisabled mon then "Enable" else "Disable"
  void $ onMenuitemItemActivated toggle $ \_ ->
    onToggleMonitor actions (monitorName mon) (not $ monitorDisabled mon)
  menuitemChildAppend parent toggle

  -- Resolution submenu
  resParent <- makeLabel "Resolution"
  menuitemPropertySet resParent "children-display" "submenu"
  forM_ (monitorAvailableModes mon) $ \mode -> do
    modeItem <- makeLabel mode
    setToggleType modeItem "radio"
    -- Mark current mode as checked
    let currentMode = T.pack (show (monitorWidth mon)) <> "x" <>
                      T.pack (show (monitorHeight mon)) <> "@" <>
                      T.pack (showFF (monitorRefreshRate mon)) <> "Hz"
    if mode == currentMode
      then setToggleState modeItem 1
      else setToggleState modeItem 0
    void $ onMenuitemItemActivated modeItem $ \_ ->
      onSetMode actions (monitorName mon) mode
    menuitemChildAppend resParent modeItem
  menuitemChildAppend parent resParent

  -- Scale submenu
  scaleParent <- makeLabel "Scale"
  menuitemPropertySet scaleParent "children-display" "submenu"
  forM_ [1.0, 1.25, 1.5, 1.75, 2.0 :: Double] $ \s -> do
    scaleItem <- makeLabel $ T.pack $ show s
    setToggleType scaleItem "radio"
    if abs (monitorScale mon - s) < 0.01
      then setToggleState scaleItem 1
      else setToggleState scaleItem 0
    void $ onMenuitemItemActivated scaleItem $ \_ ->
      onSetScale actions (monitorName mon) s
    menuitemChildAppend scaleParent scaleItem
  menuitemChildAppend parent scaleParent

  return parent

-- | Format a Double to 2 decimal places (matching hyprctl output like "240.00")
showFF :: Double -> String
showFF d =
  let n = round (d * 100) :: Int
      s = show n
      (whole, frac) = splitAt (length s - 2) s
  in if null whole then "0." ++ frac else whole ++ "." ++ frac

-- Helpers

makeLabel :: Text -> IO Menuitem
makeLabel text = do
  item <- menuitemNew
  menuitemPropertySet item "label" text
  return item

addSeparator :: Menuitem -> IO ()
addSeparator parent = do
  sep <- menuitemNew
  menuitemPropertySet sep "type" "separator"
  menuitemChildAppend parent sep
  return ()

setEnabled :: Menuitem -> Bool -> IO ()
setEnabled item enabled =
  void $ menuitemPropertySetBool item "enabled" enabled

setToggleType :: Menuitem -> Text -> IO ()
setToggleType item toggleType =
  void $ menuitemPropertySet item "toggle-type" toggleType

setToggleState :: Menuitem -> Int32 -> IO ()
setToggleState item toggleState =
  void $ menuitemPropertySetInt item "toggle-state" toggleState
