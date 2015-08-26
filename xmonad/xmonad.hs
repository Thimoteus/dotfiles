module Main where

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.FadeInactive
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Scratchpad (scratchpadSpawnActionTerminal, scratchpadManageHook)
import XMonad.StackSet (RationalRect(..))

import System.Exit
import System.IO

main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ conf xmproc `additionalKeysP` keyBinds

conf xmproc = defaultConfig { terminal = "urxvt"
                            , focusFollowsMouse = False
                            , normalBorderColor = "#000000"
                            , focusedBorderColor = "#4a708b"
                            , manageHook = manageDocks <+> manageHook defaultConfig <+> scManageHook
                            , layoutHook = avoidStruts $ layoutHook defaultConfig
                            , logHook = fadeInactiveLogHook 0.85 <+> dynamicLogWithPP xmobarPP
                              { ppOutput = hPutStrLn xmproc
                              , ppTitle = xmobarColor "green" "" . shorten 50 }
                            , modMask = mod4Mask }

scManageHook = scratchpadManageHook $ RationalRect l t w h
  where
  h = 3/10
  w = 1
  t = 0 --1 - h
  l = 1 - w

keyBinds = [ ("M-S-z", spawn "lock")
           , ("M-<Return>", spawn "urxvt -cd \"`xcwd`\"")
           , ("<Print>", spawn "scrot ~/pictures/screenshots/%Y-%m-%d-%H-%M-%S_$wx$h.png")
           , ("M-<Space>", spawn "j4-dmenu-desktop --dmenu=\"dmenu -i -nb '#262626' -nf '#9b9b9b' -sb '#222222' -fn 'lemon'\"")
           , ("M-S-q", kill)
           , ("M-S-C-q", io $ exitWith ExitSuccess)
           , ("M-M1-<Space>", sendMessage NextLayout)
           , ("M-b", sendMessage ToggleStruts)
           , ("M-`", scratchpadSpawnActionTerminal "urxvt")
           , ("<XF86ModeLock>", spawn "lock")
           , ("<XF86AudioPlay>", spawn "playerctl play-pause")
           , ("<XF86AudioNext>", spawn "playerctl next")
           , ("<XF86AudioPrev>", spawn "playerctl previous") ]
           --, ("<XF86AudioMute>", spawn "pactl set-sink-mute @DEFAULT_SINK@ toggle")
           --, ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%")
           --, ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%") ]
