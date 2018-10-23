{-# LANGUAGE FlexibleContexts #-}
import Control.Monad (when)
import Data.Foldable (concat)
import qualified Data.Map as M
import Data.Monoid ((<>))
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Core (LayoutClass)

-- XMonad contrib packages
import XMonad.Hooks.DynamicLog (statusBar)
import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (avoidStruts, AvoidStruts, docks)
import qualified XMonad.Layout.Groups as G
import XMonad.Layout.Groups.Examples (zoomRowG)
import qualified XMonad.Layout.Groups.Helpers as GH
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed (simpleTabbed)
import XMonad.Layout.ZoomRow (zoomRow, ZoomMessage(Zoom))

main :: IO ()
main = xmonad =<< xmobar (ewmh . docks $ conf) where
  conf = def
    { layoutHook = myLayout
    , modMask = mod4Mask
    , keys = myKeys <> keys def
    , handleEventHook = fullscreenEventHook
    , startupHook = addEWMHFullscreen
    }
  myLayout = smartBorders (avoidStruts collumnsOfTabs)
  collumnsOfTabs =
    renamed [CutWordsRight 2] $
      G.group (simpleTabbed ||| Mirror zoomRow) zoomRowG

xmobar :: LayoutClass l Window => XConfig l -> IO (XConfig (ModifiedLayout AvoidStruts l))
xmobar = statusBar "xmobar" def (\_ -> (,) mod4Mask xK_b)

-- Advertise fullscreen support
-- see: https://mail.haskell.org/pipermail/xmonad/2017-March/015224.html
-- and: https://github.com/xmonad/xmonad-contrib/pull/109
addNETSupported :: Atom -> X ()
addNETSupported x = withDisplay $ \dpy -> do
  r               <- asks theRoot
  a_NET_SUPPORTED <- getAtom "_NET_SUPPORTED"
  a               <- getAtom "ATOM"
  liftIO $ do
    sup <- concat <$> getWindowProperty32 dpy a_NET_SUPPORTED r
    when (fromIntegral x `notElem` sup) $
      changeProperty32 dpy r a_NET_SUPPORTED a propModeAppend [fromIntegral x]

addEWMHFullscreen :: X ()
addEWMHFullscreen = do
  wms <- getAtom "_NET_WM_STATE"
  wfs <- getAtom "_NET_WM_STATE_FULLSCREEN"
  mapM_ addNETSupported [wms, wfs]

myKeys :: XConfig l -> M.Map (ButtonMask, KeySym) (X ())
myKeys _ = M.fromList
  -- Media keys
  [ ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 5")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 5")
  , ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 4%+")
  , ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 4%-")
  , ((0, xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle")
  -- Launch programs
  , ((m, xK_Return), spawn "SHELL=/usr/bin/fish urxvt")
  , ((s, xK_Print), spawn "~/bin/screenshot")
  , ((m, xK_g), spawn "skippy-xd")
  , ((m, xK_p), spawn "rofi -show combi")
  -- Manage windows
  , ((m, xK_h), GH.focusGroupUp)
  , ((m, xK_l), GH.focusGroupDown)
  , ((m, xK_j), GH.focusDown)
  , ((m, xK_k), GH.focusUp)
  , ((m .|. s, xK_h), GH.moveToGroupUp False) -- Move to column on left
  , ((m .|. s, xK_l), GH.moveToGroupDown False) -- Move to column on right
  , ((m .|. s, xK_j), sendMessage $ G.Modify G.swapDown) -- Swap window down
  , ((m .|. s, xK_k), sendMessage $ G.Modify G.swapUp) -- Swap window up
  , ((m .|. a, xK_h), sendEnclosingMessage . Zoom $ 5/6) -- Shrink column
  , ((m .|. a, xK_l), sendEnclosingMessage $ Zoom 1.2) -- Expand column
  , ((m .|. a, xK_j), sendMessage . Zoom $ 5/6) -- Shrink window
  , ((m .|. a, xK_k), sendMessage $ Zoom 1.2) -- Expand window
  ]
  where
    m = mod4Mask
    s = shiftMask
    a = mod1Mask
    sendEnclosingMessage = sendMessage . G.ToEnclosing . SomeMessage
