import System.IO
import System.Exit
import XMonad

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.Reflect
import XMonad.Layout.IM
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Fullscreen

-- Data.Ratio for IM layout
import Data.Ratio ((%))
import Data.List (isInfixOf)

import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M


------------------------------------------------------------------------
-- Terminal
myTerminal = "/usr/bin/urxvt"


------------------------------------------------------------------------
-- Workspaces
myWorkspaces = ["term","code","web","mail","chat","music","video", "gimp", "misc"]


------------------------------------------------------------------------
-- Window rules
--
-- To find the property name associated with a program, use
-- the command xprop WM_CLASS and click on the client you're interested
-- in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "Subl3"            --> doF (W.shift "code")
    , className =? "Eclipse"          --> doF (W.shift "code")
    , className =? "Firefox"          --> doF (W.shift "web")
    , className =? "Chromium"         --> doF (W.shift "web")
    , className =? "Thunderbird"      --> doF (W.shift "mail")
    , className =? "Pidgin"           --> doShift "chat"
    , className =? "Gimp"             --> doFloat
    , className =? "mpv"              --> doFloat
    , className =? "Transmission-gtk" --> doFloat
    , resource  =? "desktop_window"   --> doIgnore
    , className =? "stalonetray"      --> doIgnore
    , isFullscreen                    --> (doF W.focusDown <+> doFullFloat)]


------------------------------------------------------------------------
-- Layout Hook
myLayoutHook = onWorkspace "chat" pidginLayout $
               spacingLayout
    where
      standardLayouts = avoidStruts(Tall 1 (3/100) (1/2) |||
                                    Mirror (Tall 1 (3/100) (1/2)))

      pidginLayout = spacing 5 $ avoidStruts(withIM (18/100) (Role "buddy_list") Grid)

      spacingLayout = (smartSpacing 5 $ standardLayouts)


------------------------------------------------------------------------
-- Colors and borders

-- Solarized theme
sBase03  = "#002b36"
sBase02  = "#073642"
sBase01  = "#586e75"
sBase00  = "#657b83"
sBase0   = "#839496"
sBase1   = "#93a1a1"
sBase2   = "#eee8d5"
sBase3   = "#fdf6e3"
sYellow  = "#b58900"
sOrange  = "#cb4b16"
sRed     = "#dc322f"
sMagenta = "#d33682"
sViolet  = "#6c71c4"
sBlue    = "#268bd2"
sCyan    = "#2aa198"
sGreen   = "#859900"

-- Some other colors
pink  = "#ffb6b0"
green = "#ceffac";

-- Border colors
myNormalBorderColor  = sBase01
myFocusedBorderColor = sGreen

-- Color of current window title in xmobar.
xmobarTitleColor = pink

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = green

-- Width of the window border in pixels.
myBorderWidth = 4


------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Lock the screen using xscreensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn "xscreensaver-command -lock")

  -- Take a screenshot in select mode.
  -- After pressing this key binding, click a window, or draw a rectangle with
  -- the mouse.
  , ((modMask .|. shiftMask, xK_p),
     spawn "select-screenshot")

  -- Take full screenshot in multi-head mode.
  -- That is, take a screenshot of everything you see.
  , ((modMask .|. shiftMask, xK_p),
     spawn "screenshot")

  -- Mute volume.
  , ((0 , 0x1008ff12),
     spawn "amixer -q set Master toggle")

  -- Decrease volume.
  , ((0 , 0x1008ff11),
     spawn "amixer -q set Master 5%-")

  -- Increase volume.
  , ((0 , 0x1008ff13),
     spawn "amixer -q set Master 5%+")

  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

--wifi (0 , 0x1008ff13)

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
  [
    -- mod-button1, Set the window to floating mode and move by dragging
    ((modMask, button1),
     (\w -> focus w >> mouseMoveWindow w))

    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2),
       (\w -> focus w >> windows W.swapMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modMask, button3),
       (\w -> focus w >> mouseResizeWindow w))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]


------------------------------------------------------------------------
-- Status bars and logging
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
-- To emulate dwm's status bar
--
-- > logHook = dynamicLogDzen
--


------------------------------------------------------------------------
-- Startup hook
-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = return ()


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--
main = do
  xmproc <- spawnPipe "xmobar ~/.xmonad/xmobar.config"
  xmonad $ defaults {
      logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc
          , ppTitle = xmobarColor xmobarTitleColor "" . shorten 50
          , ppOrder = \(ws:_:t:_) -> [ws, t]
          , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
          , ppSep = " "
      }
      , manageHook = manageDocks <+> myManageHook
      , startupHook = setWMName "LG3D"
}

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
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
