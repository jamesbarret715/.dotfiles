-- 
-- xmonad config - https://xmonad.org
-- depends on:
--   - xmobar
--

import XMonad

import XMonad.StackSet hiding (workspaces)

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.Spacing
import XMonad.Layout.Renamed
import XMonad.Layout.Grid

import XMonad.Util.EZConfig

import Data.Ratio

--- MAIN ---

main :: IO ()
main = do
    xmonad
      $ docks
      $ ewmh
      $ ewmhFullscreen
      $ withEasySB (statusBarProp "xmobar" (pure myXmobar)) defToggleStrutsKey
      $ myConfig

--- PREFERENCES ---

myTerminal     = "alacritty"
myModMask      = mod4Mask

myBorderWidth  = 1
mySpacing      = 2

--- COLOURS ---

-- Normal colors 
black   = "#282a2e"
red     = "#a54242"
green   = "#8c9440"
yellow  = "#de935f"
blue    = "#5f819d"
magenta = "#85678f"
cyan    = "#5e8d87"
white   = "#707880"

-- Bright colors
bright_black   = "#373b41"
bright_red     = "#cc6666"
bright_green   = "#b5bd68"
bright_yellow  = "#f0c674"
bright_blue    = "#81a2be"
bright_magenta = "#b294bb"
bright_cyan    = "#8abeb7"
bright_white   = "#c5c8c6"

-- Special colors
background = "#1d1f21"
foreground = "#c5c8c6"
pure_white = "#ffffff"

--- LAYOUTS ---

myLayoutHook = avoidStruts $ layout_main ||| layout_flip ||| layout_grid ||| layout_full

layout_main = renamed [Replace "main"]
            $ Tall 1 (3/100) (1/2)

layout_full = renamed [Replace "full"]
            $ Full

layout_flip = renamed [Replace "flip"]
            $ Mirror layout_main

layout_grid = renamed [Replace "grid"]
            $ Grid
--- WORKSPACES ---

myWorkspaces :: [String]
myWorkspaces = clickableWorkspaces 
    [ "home",
      "web",
      "media",
      "chat",
      "play",
      "work",
      "alpha",
      "beta",
      "gamma" ]

clickableWorkspaces :: [String] -> [String]
clickableWorkspaces workspaces = [switchWorkspace name i | (name, i) <- zip workspaces [0..length workspaces - 1]]  
  where
    switchWorkspace name i = xmobarAction ("wmctrl -s " ++ show i) "1" name

--- WINDOW RULES ---

myManageHook = composeAll
    [ className =? "feh"              --> doFloat,
      role      =? "pop-up"           --> doFloat,
      role      =? "PictureInPicture" --> doRectFloat (RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)),
      appName   =? "xmessage"         --> doRectFloat (RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)) ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

--- XMOBAR ---

myXmobar:: PP
myXmobar = def {
    ppCurrent         = xmobarColor pure_white cyan . pad,
    ppUrgent          = xmobarColor pure_white yellow . pad,
    ppVisible         = pad,
    ppHidden          = pad,
    ppHiddenNoWindows = xmobarColor white background . pad,
    ppLayout          = xmobarColor pure_white cyan . xmobarAction "xdotool key super+space" "1" . pad,
    ppTitle           = xmobarStrip . pad,
    ppWsSep           = "",
    ppSep             = " "
}

--- STARTUP ---

myStartupHook = spawn "$HOME/.config/xmonad/launch.sh"

--- CONFIG ---

myConfig = def {  
    modMask             = myModMask,
    workspaces          = myWorkspaces,
    terminal            = myTerminal,
    layoutHook          = myLayoutHook,
    startupHook         = myStartupHook,
    manageHook          = myManageHook,
    focusFollowsMouse   = False,
    clickJustFocuses    = False,
    borderWidth         = myBorderWidth,
    normalBorderColor   = bright_black,
    focusedBorderColor  = bright_black
}
