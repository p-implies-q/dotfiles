
import           XMonad

import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers

import           XMonad.Util.Run             (spawnPipe)

import           XMonad.Util.NamedScratchpad

import           XMonad.Layout.NoBorders
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.Spacing

import           Control.Monad               (when)
import           System.IO

import qualified Data.Map                    as M
import qualified XMonad.StackSet             as W


hostname :: IO String
hostname = readFile "/etc/hostname"

main :: IO ()
main = do

  xmproc <- spawnPipe "xmobar /home/david/.xmobarrc"
  xmonad $ docks $ def {
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
        logHook            = myLogHook xmproc,
        manageHook         = myManageHook
        }

myLayout = tiled
       -- ||| spacingRaw True (Border 0 10 10 10) True (Border 10 10 10 10) True tiled
          ||| spacing (10 :: Int) tiled
  where
    tiled = avoidStruts . smartBorders $ ResizableTall 1 (3/100) (1/2) []

-- myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll
    [ className =? "Inkscape"       --> doFloat
    , className =? "Matlab"         --> doFloat
    , className =? "Scribus"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Steam"          --> doFloat
    , className =? "steam"          --> doFullFloat
    , resource  =? "desktop_window" --> doIgnore
    , isDialog                      --> doCenterFloat
    , manageDocks <+> manageHook def
    , isFullscreen --> doFullFloat ]
    <+> namedScratchpadManageHook myScratchpads

myScratchpads :: [NamedScratchpad]
myScratchpads =
  [
    NS "chromium" "chromium-browser" (className =? "chromium-browser") nonFloating
  , NS "htop" "termite -e htop" (title =? "htop") nonFloating]


myTerminal          = "termite"
myFocusFollowsMouse = False
myClickJustFocuses  = False
myModMask           = mod4Mask
myWorkspaces        = map show [1..9]

myBorderWidth        = 2
myNormalBorderColor  = "#282828"
myFocusedBorderColor = "#928374"


-- emacsclient -nc -F "(quote (name . \"capture\"))" --eval "(make-capture-frame)"
captureCmd = "emacsclient -nc -F " ++ traits ++ " --eval " ++ cmd ++ " &>/tmp/err"
  where
    name   = show $ "capture"
    traits = show $ "(quote (name . " ++ name ++ "))"
    cmd    = show $ "(make-capture-frame)"

launchCmd :: String
launchCmd = "eval (yeganesh -x -- -fn 'Inconsolata:bold:pixelsize=17' -nb '#282828' -nf '#ebdbb2' -sb '#458588' -sf '#fbf1c7')"

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]


myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP $ def
   {
       ppCurrent           = \s ->  "[" ++ s ++ "]"
     , ppVisible           = \s ->  "(" ++ s ++ ")"
     , ppWsSep             = " "
     , ppSep               = "   "
     , ppLayout            = (\x -> case x of
                                   "Spacing 10 ResizableTall"-> "V"
                                   "ResizableTall"            -> ">"
                                   "Full"                     -> "^"
                                   _                          -> x
                               )
     , ppTitle             = take 40
     , ppOutput            = hPutStrLn h
   }



myKeys conf@(XConfig {XMonad.modMask = modm}) =
   M.fromList $
    let m  key cmd = ((modm,                 key), cmd)
        am key cmd = ((modm .|. mod1Mask,    key), cmd)
        sm key cmd = ((modm .|. shiftMask,   key), cmd)
        cm key cmd = ((modm .|. controlMask, key), cmd)
        s  key cmd = ((0    .|. shiftMask,   key), cmd)
        p  key cmd = ((0,                    key), cmd)
    in
      [
      -- Define a number of win-X commands
         m  xK_a         (spawn "emacs")
      , sm xK_r         (spawn "qutebrowser")
      ,  m xK_r         (spawn "chromium-browser --new-window")
      , sm  xK_s         (spawn "xterm > /home/david/error")
      , m xK_s         (spawn "termite -c `which fish`")
      , m  xK_t         (spawn "pavucontrol")
      , sm xK_t         (spawn "xmonad --recompile && xmonad --restart")
      , m  xK_Tab       (spawn "password-store")
      , m  xK_semicolon (spawn "eval $(yeganesh -x -- -fn 'mononoki Nerd Font:bold:pixelsize=15' -nb '#282828' -nf '#ebdbb2' -sb '#458588' -sf '#fbf1c7')" )
      , cm xK_q         (spawn "pymodoro")
      , m  xK_d         (kill)
      , m  xK_q         (sendMessage Shrink)
      , m  xK_w         (sendMessage Expand)
      , m  xK_f         (windows W.focusUp)
      , m  xK_p         (windows W.focusDown)
      , m  xK_g         (windows W.focusMaster)
      , sm xK_q         (sendMessage MirrorExpand)
      , sm xK_w         (sendMessage MirrorShrink)
      , sm xK_f         (windows W.swapUp)
      , sm xK_p         (windows W.swapDown)
      , sm xK_g         (windows W.swapMaster)
      , m  xK_v         (sendMessage ToggleStruts)
      , sm xK_v         (withFocused $ windows . W.sink)
      , m  xK_b         (sendMessage NextLayout)


      -- Scratchpad stuff
      , am xK_a         (namedScratchpadAction myScratchpads "chromium")
      , am xK_r         (namedScratchpadAction myScratchpads "htop")
      -- Override certain keys completely
      , p 0x1008FF11    (spawn "~/bin/pulsevolume minus")
      , p 0x1008FF13    (spawn "~/bin/pulsevolume plus")
      , p 0x1008FF12    (spawn "~/bin/pulsevolume mute")
      , p 0x1008ff02    (spawn "sudo ~/bin/brightness +33%")
      , s 0x1008ff02    (spawn "sudo ~/bin/brightness 4437")
      , p 0x1008ff03    (spawn "sudo ~/bin/brightness -33%")
      , s 0x1008ff03    (spawn "sudo ~/bin/brightness 0")
      , p xK_Home       (spawn "ezmon cycle")
      , p xK_End        (spawn "ezmon reset")
      , p xK_Insert     (spawn "toggle_touchpad")
      ] ++
    let wsKeys  = [xK_m, xK_comma, xK_period, xK_n, xK_e, xK_i, xK_l, xK_u, xK_y]
        scrKeys = [xK_j, xK_h, xK_k]
    in

      -- Define keys for workspace switching
      [((m .|. modm, k), windows $ f i) |
          (i, k) <- zip (XMonad.workspaces conf) wsKeys,
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]] ++

      -- Define keys for screen switching
      [((m .|. modm, k), screenWorkspace s >>= flip whenJust (windows . f))
          | (k, s) <- zip scrKeys [0..]
          , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
