import XMonad
import XMonad.Config.Kde
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks (ToggleStruts(..))
import qualified Data.Map as Map

main :: IO ()
main = xmonad kde4Config
  { modMask = mod4Mask
  , manageHook = manageHook kde4Config <+> myManageHook
  , keys = \c -> myKeys c `Map.union` keys kde4Config c
  }

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
