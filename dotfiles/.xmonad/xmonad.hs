import XMonad


import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Ungrab (unGrab)


import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.Fullscreen

import System.IO
import System.Exit
import System.Process (callCommand)

import Network.Socket hiding (sendAll)
import Network.Socket.ByteString.Lazy (sendAll)

import Data.ByteString.Lazy.Char8 (pack)
import Data.Monoid
import Control.Monad (when)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.Set        as S


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
        -- logHook            = myLogHook xmproc,
        -- logHook            = myLogHook2,
        logHook            = myLogHook xmproc,
        startupHook        = myStartupHook
        }

myStartupHook = do
  spawn "xsetroot -solid '#282828'"
  spawn "setxkbmap us,us -variant colemak, -option ctrl:nocaps,ctrl:nocaps"
  spawn "xcape"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ def
   {
       ppCurrent           = \s ->  "[" ++ s ++ "]"
     , ppVisible           = \s ->  "(" ++ s ++ ")"
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

myLogHook2 :: X ()
myLogHook2 = do

  dynamicLogWithPP $ def
    {
      ppCurrent       = \s -> "[" ++ s ++ "]"
    , ppVisible       = id
    , ppWsSep         = " "
    , ppSep           = "  "
    , ppLayout            = (\x -> case x of
                                   "Spacing 10 ResizableTall"-> "V"
                                   "ResizableTall"           -> ">"
                                   "Full"                    -> "^"
                                   _                         -> x
                               )
     , ppTitle             = take 40
     , ppOutput            = send
    }
  where
    send :: String -> IO ()
    send s = do
      sock <- socket AF_INET Stream 0
      setSocketOption sock ReuseAddr 1
      connect sock $ SockAddrInet 45678 $ tupleToHostAddress (127, 0, 0, 1)
      sendAll sock . pack $ s
      close sock

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


-- emacsclient -nc -F "(quote (name . \"capture\"))" --eval "(make-capture-frame)"
captureCmd = "emacsclient -nc -F " ++ traits ++ " --eval " ++ cmd ++ " &>/tmp/err"
  where
    name   = show $ "capture"
    traits = show $ "(quote (name . " ++ name ++ "))"
    cmd    = show $ "(make-capture-frame)"

-- Hopefully all this will soon be replaced with hydras
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    [
      ((modm,               xK_space ), unGrab >> spawn "scylla run main")
    , ((modm,               xK_Tab   ), unGrab >> spawn "scylla run main")
    , ((modm,               xK_a     ), unGrab >> spawn "scylla run main")
    , ((modm,               xK_s     ), spawn "termite")
    -- , ((modm,               xK_Tab   ), spawn "password-store")
    -- , ((modm .|. shiftMask, xK_Tab   ), spawn "password-store --type")
    , ((modm,            xK_semicolon), spawn captureCmd)
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

    , ((0                 , xK_Home    ), spawn "ezmon cycle")
    , ((0                 , xK_End     ), spawn "ezmon reset")
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
