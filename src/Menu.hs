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

data AppState = AppState
  { stateProfiles :: [Text]
  , stateCurrentProfile :: Maybe Text
  , stateMonitors :: [MonitorInfo]
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

  if stateKanshiConnected state
    then do
      -- Header: current profile
      header <- makeLabel $ case stateCurrentProfile state of
        Just p -> "Profile: " <> p
        Nothing -> "No active profile"
      setEnabled header False
      menuitemChildAppend root header

      addSeparator root

      -- Profile list (radio items)
      forM_ (stateProfiles state) $ \profile -> do
        item <- makeLabel profile
        setToggleType item "radio"
        if stateCurrentProfile state == Just profile
          then setToggleState item 1
          else setToggleState item 0
        void $ onMenuitemItemActivated item $ \_ ->
          onSwitchProfile actions profile
        menuitemChildAppend root item

      addSeparator root

      -- Reload config
      reloadItem <- makeLabel "Reload Config"
      void $ onMenuitemItemActivated reloadItem $ \_ ->
        onReloadConfig actions
      menuitemChildAppend root reloadItem

    else do
      disconnected <- makeLabel "kanshi not connected"
      setEnabled disconnected False
      menuitemChildAppend root disconnected

      retryItem <- makeLabel "Retry Connection"
      void $ onMenuitemItemActivated retryItem $ \_ ->
        onReloadConfig actions
      menuitemChildAppend root retryItem

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
  let label = monitorName mon <> " (" <>
              T.pack (show (monitorWidth mon)) <> "x" <>
              T.pack (show (monitorHeight mon)) <> " @ " <>
              T.pack (show (monitorScale mon)) <> "x)"
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
