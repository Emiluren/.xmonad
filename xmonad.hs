{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import XMonad
--import XMonad.Core (LayoutClass)
import qualified XMonad.StackSet as StackSet

-- XMonad contrib packages
import qualified XMonad.Actions.Navigation2D as Nav
import qualified XMonad.Hooks.DynamicLog as Log
import XMonad.Layout.LayoutScreens
import XMonad.Layout.TwoPane
import qualified XMonad.Util.EZConfig as EZ

-- TODO fix manual tiling
data Manual a = Manual deriving (Show, Read)

instance LayoutClass Manual Window where
  doLayout _layout (Rectangle x y w h) stack = do
    conf <- ask
    let halfWidth = w `div` 2
    return
      ( [ (theRoot conf, Rectangle x y halfWidth h)
        , (StackSet.focus stack, Rectangle (x + fromIntegral halfWidth) y halfWidth h)
        ]
      , Nothing
      )
  description _ = "Manual"

main :: IO ()
main = do
  let
    conf = def
      { terminal = "SHELL=/usr/bin/fish urxvt"
      , modMask = mod4Mask -- use Super instead of alt as a mod key
      --, layoutHook = Full ||| Manual
      }
    navConf =
      Nav.navigation2DP def ("k", "h", "j", "l")
        [ ("M-", Nav.windowGo)
        , ("M-S-", Nav.windowSwap)
        ]
        True
    keybindings =
      [ ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5")
      , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
      , ("<XF86AudioRaiseVolume>", spawn "amixer -D pulse sset Master 4%+")
      , ("<XF86AudioLowerVolume>", spawn "amixer -D pulse sset Master 4%-")
      , ("<XF86AudioMute>", spawn "amixer -D pulse sset Master toggle")
      , ("S-<Print>", spawn "~/usr/bin/screenshot")
      , ("M-g", spawn "skippy-xd")
      , ("M-p", spawn "rofi -show combi")
      , ("M-<Return>", spawn $ terminal conf)
      , ("M-S-<Space>", layoutSplitScreen 2 (TwoPane 0.5 0.5)) -- TODO: make this use bsp instead of twopane
      , ("M-C-S-<Space>", rescreen)
      ]

  modifiedConf <- Log.xmobar $ navConf $ EZ.additionalKeysP conf keybindings
  xmonad modifiedConf
