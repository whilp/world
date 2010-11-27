import XMonad
import XMonad.Hooks.DynamicLog

main = xmonad =<< xmobar conf
conf = defaultConfig { modMask = mod4Mask }
