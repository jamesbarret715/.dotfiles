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

-- main 

main :: IO ()
main = do
    xmonad
      $ docks
      $ ewmh
      $ ewmhFullscreen
      $ withEasySB (statusBarProp "xmobar" (pure myXmobar)) defToggleStrutsKey
      $ myConfig

-- preferences

myTerminal     = "alacritty"
myModMask      = mod4Mask

myBorderWidth  = 1
mySpacing      = 2

-- colours

black   = "#282828"
red     = "#a54242"
green   = "#d79921"
yellow  = "#d79921"
blue    = "#458588"
magenta = "#b16286"
cyan    = "#689d6a"
white   = "#a89984"

bright_black   = "#928374"
bright_red     = "#fb4934"
bright_green   = "#b8bb26"
bright_yellow  = "#fabd2f"
bright_blue    = "#83a598"
bright_magenta = "#d3869b"
bright_cyan    = "#8ec07c"
bright_white   = "#ebdbb2"

background = "#282828"
foreground = "#ebdbb2"
pure_white = "#ffffff"

underLine col = xmobarBorder "Bottom" col 3

-- layouts

myLayoutHook = avoidStruts $ layout_main ||| layout_flip ||| layout_grid ||| layout_full

layout_main = renamed [Replace "main"]
            $ Tall 1 (3/100) (1/2)

layout_full = renamed [Replace "full"]
            $ Full

layout_flip = renamed [Replace "flip"]
            $ Mirror layout_main

layout_grid = renamed [Replace "grid"]
            $ Grid


-- workspaces 

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
clickableWorkspaces = zipWith switchWorkspace [0..]
  where
    switchWorkspace i = xmobarAction ("wmctrl -s " ++ show i) "1"

-- key bindings

myKeys = 
    [ ("M-p",                    spawn "rofi -show run"),
      ("M-S-p",                  spawn "rofi -show window"),
      -- screenshot
      ("M-S-s",                  spawn "scrot -s -F \"$HOME/screenshots/scrot_%Y-%m-%d.png\""), 
      -- audio control
      ("<XF86AudioRaiseVolume>", spawn "pamixer -i 2"),
      ("<XF86AudioLowerVolume>", spawn "pamixer -d 2"),
      ("<XF86AudioMute>",        spawn "pamixer -t"),
      ("<XF86AudioPrev>",        spawn "playerctl previous"),
      ("<XF86AudioNext>",        spawn "playerctl next"),
      ("<XF86AudioPlay>",        spawn "playerctl play-pause") ]

-- managehook

myManageHook = composeAll
    [ className =? "feh"              --> doFloat,
      role      =? "pop-up"           --> doFloat,
      role      =? "PictureInPicture" --> doRectFloat (RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)),
      appName   =? "xmessage"         --> doRectFloat (RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2)) ]
  where
    role = stringProperty "WM_WINDOW_ROLE"

-- xmobar

myXmobar:: PP
myXmobar = def {
    ppCurrent          = xmobarBorder "Bottom" bright_white 3 . pad,
    ppUrgent           = xmobarBorder "Bottom" yellow  3 . pad,
    ppHidden           = pad,
    ppHiddenNoWindows  = xmobarColor bright_black background . pad,
    ppLayout           = xmobarBorder "Bottom" cyan 3 . xmobarColor cyan background . xmobarAction "xdotool key super+space" "1" . pad,
    ppTitle            = xmobarStrip . pad,
    ppWsSep            = "",
    ppSep              = ""
}

-- startup 

myStartupHook = spawn "$HOME/.config/xmonad/launch.sh"

-- config 

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
    normalBorderColor   = black,
    focusedBorderColor  = black
}   `additionalKeysP`     myKeys
