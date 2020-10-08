  -- Base
import XMonad
import System.IO (hPutStrLn)
import System.Exit (exitSuccess)
import qualified XMonad.StackSet as W

    -- Actions
import XMonad.Actions.CopyWindow (kill1, killAllOtherCopies)
import XMonad.Actions.CycleWS (moveTo, shiftTo, WSType(..), nextScreen, prevScreen)
import XMonad.Actions.GridSelect
import XMonad.Actions.MouseResize
import XMonad.Actions.Promote
import XMonad.Actions.RotSlaves (rotSlavesDown, rotAllDown)
import qualified XMonad.Actions.TreeSelect as TS
import XMonad.Actions.WindowGo (runOrRaise)
import XMonad.Actions.WithAll (sinkAll, killAll)
import qualified XMonad.Actions.Search as S

    -- Data
import Data.Char (isSpace)
import Data.Monoid
import Data.Maybe (isJust)
import Data.Tree
import qualified Data.Map as M

    -- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops  -- for some fullscreen events, also for xcomposite in obs.
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks (avoidStruts, docksEventHook, manageDocks, ToggleStruts(..))
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat, doCenterFloat, isDialog)
import XMonad.Hooks.ServerMode
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WorkspaceHistory

    -- Layouts
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.SimplestFloat
import XMonad.Layout.Spiral
import XMonad.Layout.ResizableTile
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

    -- Layouts modifiers
import XMonad.Layout.LayoutModifier
import XMonad.Layout.LimitWindows (limitWindows, increaseLimit, decreaseLimit)
import XMonad.Layout.Magnifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, MIRROR, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ShowWName
import XMonad.Layout.Spacing
import XMonad.Layout.WindowArranger (windowArrange, WindowArrangerMsg(..))
import qualified XMonad.Layout.ToggleLayouts as T (toggleLayouts, ToggleLayout(Toggle))
import qualified XMonad.Layout.MultiToggle as MT (Toggle(..))

    -- Prompt
import XMonad.Prompt
import XMonad.Prompt.Input
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Man
import XMonad.Prompt.Pass
import XMonad.Prompt.Shell (shellPrompt)
import XMonad.Prompt.Ssh
import XMonad.Prompt.XMonad
import Control.Arrow (first)

    -- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run (runProcessWithInput, safeSpawn, spawnPipe)
import XMonad.Util.SpawnOnce

dFont :: String
dFont = "xft:San Fransisco Display:bold:size=12:antialias=true:hinting=true"

dModMask :: KeyMask
dModMask = mod4Mask       -- Sets modkey to super/windows key

dTerminal :: String
dTerminal = "kitty"   -- Sets default terminal

dBrowser :: String
dBrowser = "brave-browser"

dRofi :: String
dRofi = "rofi -show run"

dBorderWidth :: Dimension
dBorderWidth = 4          -- Sets border width for windows

dNormColor :: String
dNormColor   = "#292d3e"  -- Border color of normal windows

dFocusColor :: String
dFocusColor  = "#bbc5ff"  -- Border color of focused windows


altMask :: KeyMask
altMask = mod1Mask         -- Setting this for use in xprompts

windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

dStartupHook :: X ()
dStartupHook = do
          spawnOnce "nitrogen --restore &"
          spawnOnce "compton &"
	  spawnOnce "$HOME/.xmonad/scripts/autostart.sh"
          spawnOnce "/usr/bin/emacs --daemon &"
          setWMName "LG3D"

dColorizer :: Window -> Bool -> X (String, String)
dColorizer = colorRangeFromClassName
                  (0x29,0x2d,0x3e) -- lowest inactive bg
                  (0x29,0x2d,0x3e) -- highest inactive bg
                  (0xc7,0x92,0xea) -- active bg
                  (0xc0,0xa7,0x9a) -- inactive fg
                  (0x29,0x2d,0x3e) -- active fg

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    fontName = dFont,
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

dSpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
dSpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

-- Below is a variation of the above except no borders are applied
-- if fewer than two windows. So a single window has no gaps.
dSpacing' :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
dSpacing' i = spacingRaw True (Border i i i i) True (Border i i i i) True

-- Defining a bunch of layouts, many that I don't use.
tall     = renamed [Replace "tall"]
           $ limitWindows 12
           $ dSpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
magnify  = renamed [Replace "magnify"]
           $ magnifier
           $ limitWindows 12
           $ dSpacing 8
           $ ResizableTall 1 (3/100) (1/2) []
monocle  = renamed [Replace "monocle"]
           $ limitWindows 20 Full
floats   = renamed [Replace "floats"]
           $ limitWindows 20 simplestFloat
grid     = renamed [Replace "grid"]
           $ limitWindows 12
           $ dSpacing 8
           $ mkToggle (single MIRROR)
           $ Grid (16/10)
spirals  = renamed [Replace "spirals"]
           $ dSpacing' 8
           $ spiral (6/7)
threeCol = renamed [Replace "threeCol"]
           $ limitWindows 7
           $ dSpacing' 4
           $ ThreeCol 1 (3/100) (1/2)
threeRow = renamed [Replace "threeRow"]
           $ limitWindows 7
           $ dSpacing' 4
           -- Mirror takes a layout and rotates it by 90 degrees.
           -- So we are applying Mirror to the ThreeCol layout.
           $ Mirror
           $ ThreeCol 1 (3/100) (1/2)

-- The layout hook
dLayoutHook = avoidStruts $ mouseResize $ windowArrange $ T.toggleLayouts floats $
               mkToggle (NBFULL ?? NOBORDERS ?? EOT) dDefaultLayout
             where
               -- I've commented out the layouts I don't use.
               dDefaultLayout =     tall
                                 ||| magnify
                                 ||| noBorders monocle
                                 ||| floats
                                 -- ||| grid
                                 -- ||| spirals
                                 -- ||| threeCol
                                 -- ||| threeRow

xmobarEscape :: String -> String
xmobarEscape = concatMap doubleLts
  where
        doubleLts '<' = "<<"
        doubleLts x   = [x]

dWorkspaces :: [String]
dWorkspaces = clickable . (map xmobarEscape)
               -- $ ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
               $ ["dev", "www", "sys", "doc", "vbox", "chat", "mus", "vid", "gfx"]
  where
        clickable l = [ "<action=xdotool key super+" ++ show (n) ++ "> " ++ ws ++ " </action>" |
                      (i,ws) <- zip [1..9] l,
                      let n = i ]

dManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
dManageHook = composeAll . concat $
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

dLogHook :: X ()
dLogHook = fadeInactiveLogHook fadeAmount
    where fadeAmount = 1.0

dDefaultPP = xmobarPP
  { ppTitle = xmobarColor xmobarTitleColor "" . shorten 100
  , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
  , ppSep = " | "
  }

main :: IO ()
main = do
     xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.hs"

     xmonad $ ewmh def
       { manageHook = ( isFullscreen --> doFullFloat ) <+> dManageHook <+> manageDocks,
         handleEventHook   = serverModeEventHookCmd
                               <+> serverModeEventHook
                               <+> serverModeEventHookF "XMONAD_PRINT" (io . putStrLn)
                               <+> docksEventHook,
	logHook            = dynamicLogWithPP $ dDefaultPP { ppOutput = hPutStrLn xmproc },
        modMask            = dModMask,
        terminal           = dTerminal,
        startupHook        = dStartupHook,
        layoutHook         = dLayoutHook,
        workspaces         = dWorkspaces,
        borderWidth        = dBorderWidth,
        normalBorderColor  = dNormColor,
        focusedBorderColor = dFocusColor}

      `additionalKeysP`
      -- XMonad
      [ ("M-C-r", spawn "xmonad --recompile"),
      	("M-S-r", spawn "xmonad --restart"),
	("M-S-q", io exitSuccess),
      -- Apps
        ("M-<Return>", spawn dTerminal),
	("M-d", spawn dRofi),
	("M-e", spawn "emacsclient -c -a ''"),
	("M-b", spawn dBrowser),
      -- Windows
        ("M-S-c", kill1),             -- Kill the currently focused client
        ("M-S-a", killAll),           -- Kill all windows on current workspace
      -- Floating windows
        ("M-f", sendMessage (T.Toggle "floats")),       -- Toggles my 'floats' layout
        ("M-<Delete>", withFocused $ windows . W.sink), -- Push floating window back to tile
        ("M-S-<Delete>", sinkAll),
      -- Layouts
        ("M-<Tab>", sendMessage NextLayout),              -- Switch to next layout
        ("M-C-<Up>", sendMessage Arrange),
        ("M-C-<Down>", sendMessage DeArrange),
    -- Workspaces
        ("M-.", nextScreen),  -- Switch focus to next monitor
        ("M-,", prevScreen),  -- Switch focus to prev monitor
    -- Misc
        ("<Print>", spawn "flameshot gui")]

