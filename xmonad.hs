import qualified Data.Map as M
import Data.Monoid ((<>))
import Graphics.X11.ExtraTypes.XF86
import XMonad

-- XMonad contrib packages
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.EwmhDesktops (ewmh)

-- TODO: use createNewWindow from XMonad.Util.XUtils to make a manual layout

main :: IO ()
main = do
  let
    conf = def
      { terminal = "SHELL=/usr/bin/fish urxvt"
      , modMask = mod4Mask
      , layoutHook = Full ||| Tall 1 (3/100) (1/2)
      , keys = myKeys <> keys def
      }
  xmobar (ewmh conf) >>= xmonad

myKeys :: XConfig Layout -> M.Map (ButtonMask, KeySym) (X ())
myKeys XConfig {XMonad.modMask = m} = M.fromList
      [ ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
      , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
      , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 4%+")
      , ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 4%-")
      , ((0, xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle")
      , ((s, xK_Print), spawn "~/usr/bin/screenshot")
      , ((m, xK_g), spawn "skippy-xd")
      , ((m, xK_p), spawn "rofi -show combi")
      ]
      where s = shiftMask
