# kanshi-sni: Display Profile & Monitor Management SNI

## Overview

A standalone Haskell SNI (StatusNotifierItem) tray application for managing display
profiles via kanshi and monitor settings via hyprctl. Lives in the taffybar GitHub
org.

## Architecture

### Project Structure

```
kanshi-sni/
  app/
    Main.hs                   -- Entry point, GLib main loop
  src/
    Kanshi/SNI.hs             -- SNI registration + DBus interface
    Kanshi/Config.hs          -- Parse kanshi config for profile names
    Kanshi/Varlink.hs         -- Direct Varlink IPC with kanshi daemon
    Hyprland/Monitors.hs      -- hyprctl monitor query + commands
    Menu.hs                   -- Build/rebuild DBusMenu from state
```

### Dependencies

- `status-notifier-item` - SNI registration and DBus interface
- `gi-dbusmenu` - building the popup menu
- `gi-gio` - DBus bus ownership for menu server
- `dbus` - session bus connection
- `aeson` - JSON parsing (kanshi Varlink responses, hyprctl output)
- `megaparsec` - parsing kanshi config file for profile names
- `network` - Unix socket for Varlink
- `process` - shelling out to hyprctl
- `fsnotify` - watching kanshi config for changes

### Kanshi Integration

**Config parsing** (`Kanshi/Config.hs`):
- Parse `~/.config/kanshi/config` to extract profile names
- Format: `profile <name> { ... }` blocks
- Only need names, not output rules

**Varlink IPC** (`Kanshi/Varlink.hs`):
- Connect to Unix socket at `/run/user/<uid>/fr.emersion.kanshi.wayland-<WAYLAND_DISPLAY>`
- JSON-over-socket protocol with null byte delimiters
- Methods:
  - `Status() -> (current_profile: ?string, pending_profile: ?string)`
  - `Switch(profile: string) -> ()`
  - `Reload() -> ()`
- Structured error responses: `ProfileNotFound`, `ProfileNotMatched`, `ProfileNotApplied`

### Hyprland Monitor Interaction

**Queries** (`Hyprland/Monitors.hs`):
- `getMonitors :: IO [MonitorInfo]` via `hyprctl monitors -j`
- `MonitorInfo`: name, description, width, height, scale, disabled, availableModes

**Commands** (all via `hyprctl keyword monitor`):
- Set resolution: `hyprctl keyword monitor <name>,<res>,auto,<scale>`
- Set scale: `hyprctl keyword monitor <name>,preferred,auto,<scale>`
- Disable: `hyprctl keyword monitor <name>,disabled`
- Enable: `hyprctl keyword monitor <name>,preferred,auto,1`

### SNI & Menu

**SNI registration** (`Kanshi/SNI.hs`):
- Bus name: `org.kanshi.SNI`
- Icon: `video-display`
- Tooltip: current profile name + monitor count
- Menu property pointing to DBusMenu server at `/StatusNotifierItem/Menu`

**Menu structure** (`Menu.hs`):
```
Current: docked                     (disabled label)
──────────────────────────
* docked                            (radio, click -> switch)
  laptop-only
  presentation
──────────────────────────
Reload Config                       (click -> reload + reparse)
──────────────────────────
> eDP-1 (2560x1600 @ 1.0x)         (submenu)
    [x] Enabled                     (toggle)
    > Resolution                    (submenu of radio items)
        * 2560x1600@240Hz
          2560x1600@60Hz
    > Scale                         (submenu of radio items)
          1.0
          1.25
        * 1.5
          2.0
> DP-3 (3840x2160 @ 1.5x)
    ...
```

### State Management

Single `MVar AppState`:
```haskell
data AppState = AppState
  { profiles :: [Text]
  , currentProfile :: Maybe Text
  , monitors :: [MonitorInfo]
  }
```

All actions: perform operation -> re-query state -> update MVar -> rebuild menu.

### Refresh Triggers

- After kanshi switch/reload: re-query status + monitors, rebuild menu
- After any hyprctl command: re-query monitors, rebuild menu
- On kanshi config file change (fsnotify): reparse profiles, rebuild menu

### Error Handling

- Kanshi not running: "kanshi not connected" menu item, retry on click
- Hyprland not running: hide monitors section, profiles still work
- Config parse failure: "config error" item, reload still available

### Lifecycle

1. Parse kanshi config for profile names
2. Connect to kanshi Varlink socket
3. Query kanshi status + hyprctl monitors
4. Set up fsnotify watcher on kanshi config
5. Build initial menu
6. Register SNI on DBus
7. Enter GLib main loop
