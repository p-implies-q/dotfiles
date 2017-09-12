import XMonad

-- import XMonad.Keys

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

-- import Hydras

-- import XMonad.Keys.Core

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Paste (sendKey)

-- import XMonad.Actions.Menu.Core (runMenu)
-- import XMonad.Actions.AfterDrag

import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen

import System.IO
import System.Exit

import Data.Monoid
import Control.Monad (when)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M



main :: IO ()
main = do

  xmproc <- spawnPipe "xmobar /home/david/.xmobarrc"

  xmonad $ docks def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        logHook            = myLogHook xmproc,
        startupHook        = myStartupHook
        }

myStartupHook = do
  spawn "xsetroot -solid '#282828'"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ def
   {
       ppCurrent           = id
     , ppVisible           = id
     , ppWsSep             = " "
     , ppSep               = "   "
     , ppLayout            = (\x -> case x of
                                   "Spacing 10 ResizableTall"-> "V"
                                   "ResizableTall"           -> ">"
                                   "Full"                    -> "^"
                                   _                         -> x
                               )
     , ppTitle             = take 40
     , ppOutput            = hPutStrLn h
   }


myLayout = tiled
       ||| spacing 10 tiled
       ||| noBorders (fullscreenFull Full)
  where
    tiled = avoidStruts . smartBorders $ ResizableTall 1 (3/100) (1/2) []


myManageHook = composeAll
    [ className =? "Gimp"           --> doFloat
    , className =? "Inkscape"       --> doFloat
    , className =? "Matlab"         --> doFloat
    , className =? "Scribus"        --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , manageDocks <+> manageHook def
    , isFullscreen --> doFullFloat ]


myTerminal          = "termite"
myFocusFollowsMouse = False
myClickJustFocuses  = False
myModMask           = mod4Mask
myWorkspaces        = map show [1..9]

myBorderWidth = 2
myNormalBorderColor = "#282828"
myFocusedBorderColor = "#928374"

-- Hopefully all this will soon be replaced with hydras
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      -- ((0,                  xK_Print ), runMenu mMain)
      ((modm,               xK_a     ), spawn "emacsclient -c")
    , ((modm,               xK_r     ), spawn "vivaldi")
    , ((modm,               xK_s     ), spawn "termite")
    , ((modm,               xK_t     ), spawn "yeganesh -x | /bin/sh")
    , ((modm,               xK_Tab   ), spawn "password-store --type")
    , ((modm,               xK_d     ), kill)
    , ((modm,               xK_q     ), sendMessage Shrink)
    , ((modm,               xK_w     ), sendMessage Expand)
    , ((modm,               xK_f     ), windows W.focusUp)
    , ((modm,               xK_p     ), windows W.focusDown)
    , ((modm,               xK_g     ), windows W.focusMaster)
    , ((modm .|. shiftMask, xK_q     ), sendMessage MirrorExpand)
    , ((modm .|. shiftMask, xK_w     ), sendMessage MirrorShrink)
    , ((modm .|. shiftMask, xK_f     ), windows W.swapDown  )
    , ((modm .|. shiftMask, xK_p     ), windows W.swapUp    )
    , ((modm .|. shiftMask, xK_g     ), windows W.swapMaster)


    , ((modm              , xK_x     ), spawn "touch ~/.pomodoro_session")
    , ((modm .|. shiftMask, xK_x     ), spawn "rm ~/.pomodoro_session")
    , ((modm              , xK_c     ), sendMessage (IncMasterN 1))
    , ((modm .|. shiftMask, xK_c     ), sendMessage (IncMasterN (-1)))
    , ((modm              , xK_v     ), sendMessage ToggleStruts)
    , ((modm .|. shiftMask, xK_v     ), withFocused $ windows . W.sink)
    , ((modm,               xK_b ), sendMessage NextLayout)

    , ((0                 , 0x1008FF11), spawn "pulsevolume minus")
    , ((0                 , 0x1008FF13), spawn "pulsevolume plus")
    , ((0                 , 0x1008FF12), spawn "pulsevolume mute")
    , ((0                 , 0x1008ff02), spawn "sudo brightness +33%")
    , ((shiftMask         , 0x1008ff02), spawn "sudo brightness 4437")
    , ((0                 , 0x1008ff03), spawn "sudo brightness -33%")
    , ((shiftMask         , 0x1008ff03), spawn "sudo brightness 0")
    , ((0                 , 0x1008ff41), spawn "xmonad --restart")

    ]
    ++
    [((m .|. modm, k), windows $ f i) | (i, k) <- zip (XMonad.workspaces conf) [xK_m, xK_comma, xK_period, xK_n, xK_e, xK_i, xK_l, xK_u, xK_y] , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_j, xK_h, xK_k] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]
