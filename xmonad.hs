module Main where

import XMonad
import XMonad.Config.Kde
import XMonad.ManageHook
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
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
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Layout.Spiral (spiral)


import qualified Data.Map as Map
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = xmonad kde4Config
  { modMask = mod4Mask
  , manageHook = manageHook kde4Config <+> myManageHook
  , keys = \c -> myKeys c `Map.union` keys kde4Config c
  , focusFollowsMouse = False
  , normalBorderColor = "#000000"
  , focusedBorderColor = "#19c4e1"
  , borderWidth = 0
  , layoutHook = smartBorders $ (avoidStruts . smartSpacing 2) (spiral phi) ||| noBorders (fullscreenFull Full) 
  }

phi = toRational $ 2 / (1 + sqrt 5 :: Double)

myKeys (XConfig {modMask = modMask}) = Map.fromList
  [ ((modMask, xK_b), sendMessage ToggleStruts)
  ]

myManageHook = composeAll . concat $
  [ floatsByClass
  , floatsByTitle
  , webApps
  , ircApps
  ]

floatsByClass = do
  toFloat <- ["Plasma-desktop", "plasmashell"]
  pure (className =? toFloat --> doFloat)

floatsByTitle = []

webApps = []

ircApps = []

myStartupHook :: X ()
myStartupHook = do
  sendMessage ToggleStruts
  liftIO $ threadDelay 1000000
  sendMessage ToggleStruts
