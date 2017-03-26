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
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Layout.Spiral (spiral)

import System.Exit
import System.IO

main = do
  --dzenLeft <- spawnPipe myXMonadBar
  --spawnPipe myStatusBar
  xmonad . ewmh $ flip additionalKeysP keyBinds $ conf -- dzenLeft

conf = defaultConfig { terminal = "urxvtc"
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
              NS "terminal" "urxvtc --hold -name xm-terminal -e nvim -c 'term fish'" (resource =? "xm-terminal") (customFloating $ W.RationalRect (1/4) (1/3) (1/2) (1/3)) -- -e tmux new-session -A -s sp
            -- , NS "cave" "urxvt --hold -name xm-cave -e ~/bin/cave" (resource =? "xm-cave") (customFloating $ W.RationalRect 0 (3/4) (1/4) (1/4))
  ]

myLayout = smartBorders $ (avoidStruts . smartSpacing 2) (spiral phi) ||| noBorders (fullscreenFull Full)
  where
  phi = toRational $ 2 / (1 + sqrt 5 :: Double)

myWorkspaces = [ "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX", "X" ]

myLogHook h = dynamicLogWithPP $ defaultPP
    { ppCurrent = dzenColor "#4484de" "black" . pad
    , ppVisible = dzenColor "#392f30" "black" . pad
    , ppHidden = dzenColor "#392f30" "black" . pad
    , ppHiddenNoWindows = const ""
    , ppWsSep = ""
    , ppTitle = take 100
    , ppSep = " » "
    , ppLayout = const ""
    , ppOutput = hPutStrLn h }


--j4dmenudesktop = "j4-dmenu-desktop --dmenu=\"dmenu -i -h 16 -nb 'black' -nf '#9b9b9b' -sb '#222222' -fn 'lemon:size=8'\""
j4dmenudesktop = "j4-dmenu-desktop --dmenu=\"dmenu -i -q -p '>>=' -x 450 -y 400 -h 15 -w 380 -l 0 -nb '#2b303b' -nf '#8fa1b3' -sb '#8fa1b3' -sf '#2b303b' -dim 0.8 -fn 'bitocra7:size=5'\""
dmenuStyle = init $ drop 32 j4dmenudesktop
passmenu = "passmenu " ++ dmenuStyle

keyBinds = [ ("M-<Return>", spawn "urxvtc -cd \"`xcwd`\" -e fish")
           -- , ("M-t", spawn "urxvt -cd \"`xcwd`\" -e fish")
           , ("<Print>", spawn "scrot ~/Pictures/screenshots/%Y-%m-%d-%H-%M-%S_$wx$h.png")
           , ("M-<Space>", spawn j4dmenudesktop)
           , ("M-q", spawn "xmonad --recompile && xmonad --restart")
           , ("M-S-q", kill)
           , ("M-S-C-q", io $ exitWith ExitSuccess)
           , ("M-M1-<Space>", sendMessage NextLayout)
           , ("M-n", spawn "notify-send \"`date +%T`\"")
           , ("M-b", spawn "notify-send \"`acpi --battery`\"")
           --, ("M-b", sendMessage ToggleStruts)
           --, ("M-a", currentDesktop >>= \ s -> spawn ("notify-send \"Current workspace: " ++ s ++ "\""))
           , ("M-r", spawn "~/bin/redshiftoggle")
           , ("M-y", toggleFocusFloat)
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
           , ("<XF86Launch1>", spawn "pulseaudio-ctl mute") -- "pactl set-sink-mute @DEFAULT_SINK@ toggle")
           , ("<XF86AudioRaiseVolume>", spawn "pulseaudio-ctl up") -- "pactl set-sink-volume @DEFAULT_SINK@ +5%")
           , ("<XF86AudioLowerVolume>", spawn "pulseaudio-ctl down") ] --"pactl set-sink-volume @DEFAULT_SINK@ -5%") ]


