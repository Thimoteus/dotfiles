module Main where

import XMonad
import XMonad.ManageHook
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Util.Run
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Loggers (logCurrent)
import XMonad.Hooks.EwmhDesktops
import qualified XMonad.StackSet as W

import XMonad.Util.Themes
import XMonad.Layout.Fullscreen
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.EqualSpacing

import XMonad.Actions.CycleWS

import System.Exit
import System.IO

main = do
  --dzenLeft <- spawnPipe myXMonadBar
  --spawnPipe myStatusBar
  xmonad . ewmh $ flip additionalKeysP keyBinds $ conf -- dzenLeft

--conf dzenLeft = defaultConfig { terminal = "urxvt"
conf = defaultConfig { terminal = "urxvt"
                     , focusFollowsMouse = False
                     , normalBorderColor = "#000000"
                     , focusedBorderColor = "#19c4e1"
                     , borderWidth = 0
                     , workspaces = myWorkspaces
                     , manageHook = foldMap id [ manageDocks
                       , manageHook defaultConfig
                       , namedScratchpadManageHook scratchpads
                       , fullscreenManageHook
                       -- , isFullscreen --> doFullFloat
                     ]
                     , layoutHook = myLayout
                     , logHook = fadeInactiveLogHook 0.6 -- >> myLogHook dzenLeft
                     , modMask = mod4Mask }

currentDesktop :: X String
currentDesktop = do
  werk <- logCurrent
  return $ maybe "???" id werk

myXMonadBar = "dzen2 -x '0' -y '0' -h '16' -w '500' -ta l"
myStatusBar = "cave | dzen2 -p -e - -h '16' -ta r -x 400 -w 880 -y 0 -fn 'lemon:size=8'"

scratchpads = [
              NS "terminal" "urxvt --hold -name xm-terminal -e tmux new-session -A -s sp" (resource =? "xm-terminal") (customFloating $ W.RationalRect (1/4) (1/3) (1/2) (1/3))
  ]

myLayout = (avoidStruts . smartBorders) (equalSpacing 6 6 0 1 emptyBSP) ||| noBorders (fullscreenFull Full)

myWorkspaces = [ "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X" ]

j4dmenudesktop = "j4-dmenu-desktop --dmenu=\"dmenu -i -fn lemon -p '>>='\""
dmenuStyle = init $ drop 32 j4dmenudesktop
passmenu = "passmenu " ++ dmenuStyle

keyBinds = [ ("M-<Return>", spawn "urxvt -e fish")
           , ("<Print>", spawn "scrot ~/Pictures/screenshots/%Y-%m-%d-%H-%M-%S_$wx$h.png")
           , ("M-<Space>", spawn j4dmenudesktop)
           , ("M-q", spawn "xmonad --recompile && xmonad --restart")
           , ("M-S-q", kill)
           , ("M-S-C-q", io $ exitWith ExitSuccess)
           , ("M-M1-<Space>", sendMessage NextLayout)
           , ("M-n", spawn "notify-send \"`date +%T`\"")
           , ("M-b", spawn "notify-send \"`acpi --battery`\"")
           , ("M-r", spawn "~/bin/redshiftoggle")
           , ("M-y", toggleFocusFloat)
           , ("C-M1-l", spawn "lock")
           , ("M-`", namedScratchpadAction scratchpads "terminal")
           , ("M-p", spawn passmenu)
           , ("M-[", sendMessage LessSpacing)
           , ("M-]", sendMessage MoreSpacing)
           , ("M-M1-h", sendMessage $ MoveSplit L)
           , ("M-M1-l", sendMessage $ MoveSplit R)
           , ("M-M1-k", sendMessage $ MoveSplit U)
           , ("M-M1-j", sendMessage $ MoveSplit D)
           , ("M-v", sendMessage Rotate)
           , ("M-c", spawn "~/bin/comptoggle")
           , ("<XF86Battery>", spawn "notify-send \"`acpi --battery`\"")
           , ("<XF86ScreenSaver>", spawn "/usr/bin/lock")
           , ("<XF86AudioPlay>", spawn "playerctl play-pause || mocp --toggle-pause")
           , ("<XF86AudioNext>", spawn "playerctl next || mocp --next")
           , ("<XF86AudioPrev>", spawn "playerctl previous || mocp --previous")
           , ("<XF86AudioStop>", spawn "playerctl stop || mocp --stop")
           , ("<XF86Sleep>", spawn "systemctl suspend")
           , ("M-<F1>", prevWS)
           , ("M-<F2>", nextWS)
           , ("M-<F6>", spawn "light -U 2")
           , ("M-<F7>", spawn "light -A 2")
           , ("M-<F8>", spawn "pulseaudio-ctl mute")
           , ("M-<F10>", spawn "pulseaudio-ctl up")
           , ("M-<F9>", spawn "pulseaudio-ctl down") ]


