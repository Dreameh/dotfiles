--
--
-- Dreameh's first Xmonad config file.
--

-- Imports

-- System
import Data.Semigroup ((<>))
import System.IO
import System.Exit
import XMonad
import XMonad.Config.Desktop

-- Utilities
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Util.SpawnOnce

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap

-- Layout
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Spacing
import XMonad.Layout.Gaps
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.IndependentScreens
import XMonad.Layout.OneBig

import XMonad.Prompt
import XMonad.Prompt.Shell
import Graphics.X11.ExtraTypes.XF86

-- Datatypes
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified DBus as D
import qualified DBus.Client as D
import qualified Codec.Binary.UTF8.String as UTF8



-- Preferred Terminal
myTerminal    = "kitty"

-- Preferred Web Browser
myWebBrowser  = "vivaldi-stable" 

-- Preferred Editor
myEditor      = "emacs"

-- Xmobar launch command
myBar = "xmobar"

-- Default ModMask
myModMask     = mod4Mask

-- Default Launcher
myLauncher = "rofi -show run"

-- Number of Workspaces
-- You can change the workspace names to be taggable
--
myWorkspaces = ["IM", "code", "internet"] ++ map show ([4..9] ++ [0])

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the tomorrow night eighties theme.
--
myNormalBorderColor  = currentline
myFocusedBorderColor = "#5e81ac" 
myBorderWidth = 4

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    fontName = "xft:Source Code Pro:size=12",
    decoHeight = 40,
    activeBorderColor   = aqua,
    activeTextColor     = foreground,
    activeColor         = selection,
    inactiveBorderColor = selection,
    inactiveTextColor   = comment,
    inactiveColor       = background
}

-- Color of current window title in xmobar.
xmobarTitleColor = background 

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = purple

-- Tomorrow Night Eighties theme
background = "#2d2d2d"
currentline = "#393939"
selection = "#515151"
foreground = "#cccccc"
comment = "#999999"
red = "#f2777a"
orange = "#f99157"
yellow = "#ffcc66"
green = "#99cc99"
brightGreen = "#77ee77"
aqua = "#66cccc"
blue = "#6699cc"
purple = "#cc99cc"
-------------------------------------------------------------------------------
-- Keybindings
--
myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

-- General Settings

  -- Printscreen(spectacle)
  [((0, xK_Print), spawn $ "spectacle")

  -- Printscreen(selective)
  , ((modMask, xK_y), spawn "sleep 0.2; scrot -s")

  -- Volume Control
  , ((controlMask .|. shiftMask, xK_u), spawn $ "pavucontrol")
  
-- Launch of Applications
  
  -- Launch Terminal
  , ((modMask, xK_Return), spawn $ XMonad.terminal conf)

  -- Launch Rofi
  , ((modMask, xK_d), spawn $ myLauncher)

  -- Open networkManager
  , ((modMask, xK_F1), spawn $ "networkmanager_dmenu")

  -- Web Browser
  , ((modMask, xK_f), spawn $ myWebBrowser)

  -- Editor
  , ((mod1Mask .|. shiftMask, xK_e), spawn  myEditor)

-- XMonad related keybindings
  
  -- Recompile and Restart Xmonad
  , ((modMask .|. shiftMask, xK_r), spawn $ "xmonad --recompile && xmonad --restart")

  -- Exit XMonad
  , ((modMask .|. shiftMask, xK_x), io (exitWith ExitSuccess))

  -- Close focused window
  , ((modMask .|. shiftMask, xK_q), kill)

  -- Lock the Screen
  , ((controlMask .|. modMask, xK_l), spawn $ "slimlock")

  -- Rotate through the available layout algorithms
  , ((modMask, xK_space), sendMessage NextLayout)

  -- Toggle status bar
  , ((modMask, xK_b), sendMessage ToggleStruts)
  ]
  ++

  --
  -- mod-[1..9] Switch to workspace N
  -- mod-shift-[1..9] Move window to workspace N
  --
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
 ++
 --
 -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
 -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
 --
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
 
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
 
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
 
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
 
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]
------------------------------------------------------------------------
-- Layouts:
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--

myLayoutHook = avoidStruts $ (threecol ||| tall ||| tabbed') ||| distractionFree
    where tabbed' = tabbed shrinkText tabConfig
          threecol = ThreeColMid 1 (3/100) (1/3)
          tall = Tall 1 (3/100) (2/3)
          grid = GridRatio (5/2)
distractionFree = noBorders (fullscreenFull Full)

------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll . concat $
               [ [isDialog --> doCenterFloat]
               , [className =? c --> doCenterFloat | c <- myCFloats]
               , [title =? t --> doFloat | t <- myTFloats]
               , [resource =? r --> doFloat | r <- myRFloats]
               , [resource =? i --> doIgnore | i <- myIgnores]
               ]
  where
    
  myCFloats = ["Arandr", "Galculator", "Oblogout", "feh", "mpv"]
  myTFloats = ["Downloads", "Save As..."]
  myRFloats = []
  myIgnores = ["desktop_window"]
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--
myDefaultPP = xmobarPP
  { ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
  , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
  , ppSep = " | "
  }

--myLogHook :: D.Client -> PP
--myLogHook dbus = def
--                 { ppOutput = dbusOutput dbus
--                 , ppCurrent = wrap ("%{B}" ++ bg2 ++ " } ") "%{B-}"
------------------------------------------------------------------------
-- Startup hook
 
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do 
  spawnOnce "$HOME/.xmonad/scripts/autostart.sh"
  setWMName "UmikoWM"

 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
  xmproc   <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"
  xmonad $ ewmh $ defaults
    { logHook    = dynamicLogWithPP $ myDefaultPP { ppOutput = hPutStrLn xmproc }
    , manageHook = manageDocks <+> myManageHook
    , handleEventHook = docksEventHook
}

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will 
-- use the defaults defined in xmonad/XMonad/Config.hs
-- 
-- No need to modify this.
--
defaults = defaultConfig {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
 
      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
 
      -- hooks, layouts
        layoutHook         = smartBorders $ myLayoutHook,
        manageHook         = myManageHook,
        startupHook        = myStartupHook
}
