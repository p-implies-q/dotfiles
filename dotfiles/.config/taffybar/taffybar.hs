-- -*- mode:haskell -*-
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.Taffybar
import System.Taffybar.Context (TaffyIO)
import System.Taffybar.Hooks
import System.Taffybar.Information.CPU
import System.Taffybar.Information.Memory
import System.Taffybar.SimpleConfig
import System.Taffybar.Widget
import System.Taffybar.Widget.Generic.PollingGraph
import System.Taffybar.Widget.Generic.PollingLabel
import System.Taffybar.Widget.Util
import System.Taffybar.Widget.Workspaces

import qualified GI.Gtk as Gtk

type RGBA = (Double, Double, Double, Double)

padding = 5


-- | Set up the colors used for the graphs

col :: (Double, Double, Double) -> RGBA
col (a, b, c) = (a / 255, b / 255, c / 255, 1.0)

bg      = col (0x28, 0x28, 0x28)
dYellow = col (0xd7, 0x99, 0x21)
lYellow = col (0xfa, 0xbd, 0x2f)
dBlue   = col (0x45, 0x85, 0x88)
lBlue   = col (0x83, 0xa5, 0x98)
dAqua   = col (0x68, 0x9d, 0x6a)
lAqua   = col (0x8e, 0xc0, 0x7c)


myGraphConfig =
  defaultGraphConfig
  { graphPadding         = padding
  , graphBorderWidth     = 0
  , graphWidth           = 75
  , graphBackgroundColor = bg
  , graphDirection       = RIGHT_TO_LEFT
  }

netCfg = myGraphConfig
  { graphDataColors = [lYellow, dYellow]
  , graphLabel      = Just "net"
  }

memCfg = myGraphConfig
  { graphDataColors = [dBlue]
  , graphLabel      = Just "mem"
  }

cpuCfg = myGraphConfig
  { graphDataColors = [lAqua, dAqua]
  , graphLabel = Just "cpu"
  }

noteConf = defaultNotificationConfig {notificationMaxTimeout = Just 5}

myWorkspaceCfg =
  defaultWorkspacesConfig
  { minIcons        = 0
  , maxIcons        = Just 3 
  , showWorkspaceFn = hideEmpty
  }

myLayoutConfig = defaultLayoutConfig
  { formatLayout = \s -> return $ case s of
      "ResizableTall"         -> ">>"
      "Spacing ResizableTall" -> "> >"
      "Full"                  -> "| |"
      s                       -> s
  }

memCallback :: IO [Double]
memCallback = do
  mi <- parseMeminfo
  return [memoryUsedRatio mi]

cpuCallback = do
  (_, systemLoad, totalLoad) <- cpuLoad
  return [totalLoad, systemLoad]

pomo :: TaffyIO Gtk.Widget
pomo = commandRunnerNew 0.5 "python" ["/home/david/opt/pymodoro/pymodoro.py", "--auto-hide", "--one-line"] "Woops"

main = do
  let
      workspaces = workspacesNew myWorkspaceCfg
      cpu = pollingGraphNew cpuCfg 0.5 cpuCallback
      mem = pollingGraphNew memCfg 1 memCallback
      net = networkGraphNew netCfg Nothing
      clock = textClockNew Nothing " %a %m-%d:%H%M " 1
      layout = layoutNew myLayoutConfig
      windows = windowsNew defaultWindowsConfig

      myConfig = defaultSimpleTaffyConfig
        { startWidgets =
            workspaces : map (>>= buildContentsBox) [ layout, windows ]
        , centerWidgets = map (>>= buildContentsBox)
          [ pomo
          , notifyAreaNew noteConf
          ]
        , endWidgets = map (>>= buildContentsBox)
          [ clock
          -- , tray
          , cpu
          , mem
          , net
          , mpris2New
          ]
        , barPosition = Top
        , barPadding = 0
        , barHeight = 22
        , widgetSpacing = 5
        }
  dyreTaffybar $ withLogServer $ withToggleServer $
               toTaffyConfig myConfig
