import System.IO
import System.Exit
import XMonad
import Dzen
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
import XMonad.Layout.PerWorkspace (onWorkspace, onWorkspaces)
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.Fullscreen

-- Data.Ratio for IM layout
import Data.Ratio ((%))
import Data.List (isInfixOf)

import XMonad.Actions.CycleWS (nextWS, prevWS)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig (additionalKeys)
import Control.Monad (liftM2)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Char (isSpace)

------------------------------------------------------------------------
-- Terminal
--

myTerminal = "/usr/bin/urxvt"


------------------------------------------------------------------------
-- Workspaces
--

myWorkspaces = ["main","text","ide","web","mail","chat","media","gimp","misc"]


------------------------------------------------------------------------
-- Window rules
--

myManageHook = composeAll . concat $
  [
    -- Applications that go to text.
      [ className =? b --> viewShift "text" | b <- myClassTextShifts ]

    -- Applications that go to ide.
    , [ className =? c --> viewShift "ide" | c <- myClassDevShifts ]

    -- Applications that go to web.
    , [ className =? d --> viewShift "web" | d <- myClassWebShifts ]

    -- Applications that go to mail.
    , [ className =? e --> viewShift "mail" | e <- myClassMailShifts ]

    -- Applications that go to chat.
    , [ className =? f --> viewShift "chat" | f <- myClassChatShifts ]

    -- Applications that go to media.
    , [ className =? g --> viewShift "media" | g <- myClassMediaShifts ]

    -- Applications that need floating regardless of workspace.
    , [ className =? h --> doCenterFloat | h <- myClassFloats ]
    , [ resource  =? i --> doCenterFloat | i <- myResourceFloats ]

    -- Applications that need to be ignored.
    , [ className =? j --> doIgnore | j <- myClassIgnores ]
    , [ resource  =? k --> doIgnore | k <- myResourceIgnores ]

    , [ composeOne [ isFullscreen -?> (doF W.focusDown <+> doFullFloat) ] ]
  ]
  where
      viewShift          = doF . liftM2 (.) W.greedyView W.shift
      myClassWebShifts   = ["Firefox","Chromium"]
      myClassChatShifts  = ["Pidgin", "Skype"]
      myClassMediaShifts = ["mpv"]
      myClassTextShifts  = ["Subl3"]
      myClassDevShifts   = ["eclipse"]
      myClassMailShifts  = ["Thunderbird"]
      myClassFloats      = ["feh"]
      myResourceFloats   = ["Downloads", "Dialog", "Places", "Browser"]
      myClassIgnores     = ["stalonetray"]
      myResourceIgnores  = ["desktop_window"]


------------------------------------------------------------------------
-- Layout Hook
--

myLayoutHook = onWorkspace "chat" chatLayout $
               onWorkspace "gimp" gimpLayout $
               spacingLayout
    where
      standardLayouts = avoidStruts(Tall 1 (3/100) (1/2) ||| Mirror (Tall 1 (3/100) (1/2)))
      chatLayout      = spacing 2 $ avoidStruts(IM (1%5) (Or (Title "Buddy List") (And (Resource "main") (ClassName "pidgin"))))
      gimpLayout      = spacing 2 $ avoidStruts((withIM (0.12) (Role "gimp-toolbox") $ reflectHoriz $ withIM (0.15) (Role "gimp-dock") Full))
      spacingLayout   = (smartSpacing 2 $ standardLayouts)


------------------------------------------------------------------------
-- Colors and borders
--

-- Custom theme colors
red   = "#ff0000"
pink  = "#ff807a"
green = "#ceffac"
white = "#cfcfcf"
black = "#000000"

-- Border colors
myNormalBorderColor  = white
myFocusedBorderColor = pink

-- Width of the window border in pixels.
myBorderWidth = 1


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

  -- Launch dmenu.
  , ((modMask, xK_p),
     spawn "dmenu_run -fn 'DejaVu Sans Mono' -h 24 -nb '#000000' -nf '#cfcfcf' -sb '#ff807a' -sf '#000000'")

  -- Lock the screen using xscreensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn "xscreensaver-command -lock")

  -- Take a screenshot in select mode.
  -- After pressing this key binding, click a window, or draw a rectangle with
  -- the mouse.
  , ((modMask .|. shiftMask, xK_s),
     spawn "select-screenshot")

  -- Take full screenshot in multi-head mode.
  -- That is, take a screenshot of everything you see.
  , ((modMask .|. shiftMask, xK_s),
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
     spawn "ncmpcpp prev")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "ncmpcpp toggle")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "ncmpcpp next")

  -- Eject CD tray.
  , ((0, 0x1008FF2C),
     spawn "eject -T")

  -- Rebind mod + q: custom restart xmonad script
  , ((modMask, xK_q),
     spawn "killall dzen2 && xmonad --recompile && xmonad --restart")

  --------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Shift to prevous workspace.
  , ((modMask, xK_Right),
     nextWS)

  -- Shift to next workspace.
  , ((modMask, xK_Left),
     prevWS)

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
--

-- Log hook that prints out everything to a dzen handler.
myLogHook h = dynamicLogWithPP $ myPrettyPrinter h

-- Pretty printer for dzen workspace bar.
myPrettyPrinter h = dzenPP
  {
    ppOutput          = hPutStrLn h
  , ppCurrent         = dzenColor black pink . pad
  , ppHidden          = dzenColor "#e5e5e5" black . pad . clickable myWorkspaces . trimSpace
  , ppHiddenNoWindows = dzenColor "#444444" black . pad . clickable myWorkspaces . trimSpace
  , ppUrgent          = dzenColor "#ff0000" red . pad . clickable myWorkspaces . trimSpace . dzenStrip
  , ppWsSep           = " "
  , ppSep             = " | "
  , ppTitle           = (" " ++) . dzenColor pink black . shorten 120 . dzenEscape
  , ppLayout          = dzenColor green black . pad .
                        (\x -> case x of
                          "SmartSpacing 2 Tall"        -> "Tall"
                          "SimplestFloat"              -> "Float"
                          "SmartSpacing 2 Mirror Tall" -> "Mirror"
                          _                            -> x
                        )
  }

-- Wraps a workspace name with a dzen clickable action that focusses that workspace.
clickable workspaces workspace = clickableExp workspaces 1 workspace

clickableExp [] _ ws = ws
clickableExp (ws:other) n l | l == ws = "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()"
                            | otherwise = clickableExp other (n+1) l

-- Trims leading and trailing white space.
trimSpace = f . f
    where f = reverse . dropWhile isSpace

myDzenFont = "DejaVu Sans Mono:pixelsize=12"

-- Workspace dzen bar
myWorkDzen = DzenConf {
    x_position = Just 0
  , y_position = Just 0
  , width      = Just 1820
  , height     = Just 22
  , alignment  = Just LeftAlign
  , font       = Just myDzenFont
  , fg_color   = Just white
  , bg_color   = Just black
  , exec       = []
  , addargs    = []
}

-- Music dzen bar
myMusicDzen = DzenConf {
    x_position = Just 0
  , y_position = Just 1080
  , width      = Just 700
  , height     = Just 24
  , alignment  = Just LeftAlign
  , font       = Just myDzenFont
  , fg_color   = Just pink
  , bg_color   = Just black
  , exec       = []
  , addargs    = []
}

-- System information dzen bar
mySysInfoDzen = DzenConf {
    x_position = Just 700
  , y_position = Just 1080
  , width      = Just 1220
  , height     = Just 24
  , alignment  = Just RightAlign
  , font       = Just myDzenFont
  , fg_color   = Just pink
  , bg_color   = Just black
  , exec       = []
  , addargs    = []
}

------------------------------------------------------------------------
-- Startup hook
-- By default, do nothing.
--
myStartupHook = return ()


------------------------------------------------------------------------
-- Run xmonad with all the defaults we set up.
--

main = do
  workspaceBar <- spawnDzen myWorkDzen
  spawnToDzen "conky -c ~/.conkyrc-sysinfo" mySysInfoDzen
  spawnToDzen "conky -c ~/.conkyrc-music" myMusicDzen
  xmonad $ defaults {
        logHook = myLogHook workspaceBar
      , manageHook = manageDocks <+> myManageHook
      , startupHook = setWMName "LG3D"
}

------------------------------------------------------------------------
-- Combine it all together
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
