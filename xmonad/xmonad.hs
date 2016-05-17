module Main where

import Data.Monoid
import Data.List (isInfixOf)

import XMonad
import XMonad.Actions.WindowGo (raise, doF, className)
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
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Groups.Helpers
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Layout.Spiral (spiral)

import XMonad.Actions.CycleWS

import System.Exit
import System.IO

main = do
  bar <- spawnPipe myXMonadBar
  xmonad . ewmh $ additionalKeysP (conf bar) keyBinds

conf b =
  defaultConfig { terminal = "konsole"
                , focusFollowsMouse = False
                , normalBorderColor = "#000000"
                , focusedBorderColor = "#19c4e1"
                , borderWidth = 0
                , handleEventHook = docksEventHook
                , manageHook = namedScratchpadManageHook scratchpads
                  <> fullscreenManageHook
                  <> (appName =? "wingpanel" --> doIgnore)
                  <> manageDocks
                , layoutHook = myLayout
                , startupHook = myStartupHook
                , logHook = fadeInactiveLogHook 0.6
                         <> dynamicLogWithPP (defaultPP { ppOutput = hPutStrLn b
                                                        , ppCurrent = \ x -> lemonColor "#53FFFF" $ getWorkspaceName x
                                                        , ppHidden = \ x -> case x of
                                                                              "NSP" -> ""
                                                                              _ -> getWorkspaceName x
                                                        , ppHiddenNoWindows = \ x ->
                                                          case x of
                                                              "NSP" -> ""
                                                              y -> lemonColor "#0b3c4d" $ getWorkspaceName y
                                                        , ppTitle = renderTitle
                                                        , ppLayout = const ""
                                                        , ppOrder = alignLemonbarSections
                                                        , ppSep = ""
                                                        , ppWsSep = " "
                                                        })
                , modMask = mod4Mask }

getWorkspaceName y = y --workspaceNames !! r y
  where
  r "1" = 0
  r "2" = 1
  r "3" = 2
  r "4" = 3
  r "5" = 4
  r "6" = 5
  r "7" = 6
  r "8" = 7
  r "9" = 8
  r _ = 0
  workspaceNames = [ "α", "β", "γ", "δ", "ε", "ζ", "η", "θ", "ι", "κ" ]

alignLemonbarSections :: [String] -> [String]
alignLemonbarSections (x:xs) = xs ++ ["%{r}" <> x]
alignLemonbarSections _ = []

renderTitle :: String -> String
renderTitle = take 160 . unwords . map f . words
  where
  f s | "/home/archurito" `isInfixOf` s = "~" <> drop 15 s
  f s = s

lemonColor :: String -> String -> String
lemonColor c str = "%{F" <> c <> "}" <> str <> "%{F-}"

myStartupHook = do
  spawn $ "node ~/dotfiles/xmonad/right_script | " <> myLemonBar 643 723

myXMonadBar = myLemonBar 723 0

-- (width, x-pos) -> lemonbar
myLemonBar :: Int -> Int -> String
myLemonBar w x = "lemonbar -p -B '#AA000000' -o -4 -g " <> show w <> "x9+" <> show x <> " -f '-*-bitocra7-*-*-*-*-*-*-*-*-*-*-*-*' -f '-*-creep-*-*-*-*-*-*-*-*-*-*-*-*'"

scratchpads = [
              NS "terminal" "urxvt --hold -name xm-terminal -e tmux new-session -A -s sp" (resource =? "xm-terminal") (customFloating $ W.RationalRect (1/4) (1/3) (1/2) (1/3))
  ]

--myLayout = smartBorders (equalSpacing 6 6 0 1 emptyBSP) ||| noBorders (fullscreenFull Full)
myLayout = smartBorders $ (avoidStruts . smartSpacing 2) (spiral phi) ||| noBorders (fullscreenFull Full)
  where
  tall = (ResizableTall 1 (3/100) (1/2) [])
  phi = toRational $ 2 / (1 + sqrt 5 :: Double)

j4dmenudesktop = "j4-dmenu-desktop --dmenu=\"dmenu -i -b -dim 0.5 -fn lemon-7 -p '>>='\""
dmenuStyle = init $ drop 32 j4dmenudesktop
passmenu = "passmenu " ++ dmenuStyle

keyBinds = [ ("M-<Return>", spawn "konsole")
           , ("<Print>", spawn "scrot ~/Pictures/screenshots/%Y-%m-%d-%H-%M-%S_$wx$h.png")
           , ("M-<Space>", spawn j4dmenudesktop)
           , ("M-q", spawn "xmonad --recompile && killall lemonbar && xmonad --restart")
           , ("M-S-q", kill)
           , ("M-S-C-q", io $ exitWith ExitSuccess)
           , ("M-M1-<Space>", sendMessage NextLayout)
           , ("M-n", spawn "notify-send \"time\" \"`date +%T`\"")
           , ("M-b", spawn "notify-send \"battery status\" \"`acpi --battery`\"")
           , ("M-y", toggleFocusFloat)
           , ("C-M1-l", spawn "lock")
           , ("M-`", namedScratchpadAction scratchpads "terminal")
           , ("M-p", spawn passmenu)
           , ("M-c", spawn "~/bin/comptoggle")
           , ("<XF86Battery>", spawn "notify-send \"`acpi --battery`\"")
           , ("<XF86ScreenSaver>", spawn "/usr/bin/lock")
           , ("M-<F1>", prevWS)
           , ("M-<F2>", nextWS)
           , ("M-<F6>", spawn "light -U 2")
           , ("M-<F7>", spawn "light -A 2")
           , ("M-<F8>", spawn "~/bin/sound_mute")
           , ("M-<F9>", spawn "~/bin/sound_down")
           , ("M-<F10>", spawn "~/bin/sound_up")
           ]


