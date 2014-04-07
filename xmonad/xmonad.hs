---------------------------------------
-- XMonad configuration
---------------------------------------
-- Lots of general stuff taken from the default config and earsplit's XMonad config
-- https://github.com/windelicato/windelicato.github.com
-- GridSelect config from http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Nnoell%27s_xmonad.hs

-- Options
{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction, MultiParamTypeClasses, ImplicitParams #-}

import XMonad
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Layout
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Fullscreen hiding (fullscreenEventHook)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.Circle
import XMonad.Layout.Spiral

import XMonad.Actions.CycleWS
import XMonad.Actions.GridSelect

import XMonad.ManageHook
import XMonad.Hooks.ManageHelpers

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run (spawnPipe, hPutStrLn)
import XMonad.Util.Loggers
--import XMonad.Hooks.DynamicLog hiding (sjanssenPP, byorgeyPP)
--import XMonad.Hooks.DynamicLog (PP(..), dynamicLogWithPP, wrap, pad)

import XMonad.Prompt
import XMonad.Prompt.Shell hiding (getShellCompl)
import XMonad.Prompt.Man

import Graphics.X11.ExtraTypes.XF86
import XMonad.Hooks.EwmhDesktops-- hiding (fullscreenEventHook)
import XMonad.Hooks.SetWMName  -- for ImageJ and other java softwares

import System.IO

main = do
    h <- spawnPipe myXmonadBar
    --bottomBar <- spawnPipe myBottomBar
    xmonad $ ewmh defaultConfig
        { terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , modMask            = myModMask
        , borderWidth        = myBorderWidth
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , keys               = myKeys
        , manageHook         = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook         = smartBorders $ myLayout
        , logHook            = myLogHook h
        , handleEventHook    = handleEventHook defaultConfig <+> docksEventHook <+> fullscreenEventHook
        }

---------------------------------------
-- General settings and variables
---------------------------------------
-- The preferred terminal program
myTerminal = "/usr/bin/termite"
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False
-- Width of the window border in pixels.
myBorderWidth = 4
-- Border colors for unfocused and focused windows, respectively.
myNormalBorderColor = background
myFocusedBorderColor = foreground
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
myModMask = mod4Mask
-- Colors & fonts
--myFont          = "-*-terminus-*-*-*-*-12-*-*-*-*-*-*-*"
myFont = "-*-nu.de-*-*-*-*-11-*-*-*-*-*-*-*"

background  = "#0D0B0D"
foreground  = "#F4F3F5"
cursorColor = "#F3DCC6"
color0      = "#1E2F41"
color8      = "#906B61"
color1      = "#84252C"
color9      = "#C92831"
color2      = "#87A581"
color10     = "#C0BD86"
color3      = "#D6C08F"
color11     = "#F1DEB7"
color4      = "#143A58"
color12     = "#1A4C72"
color5      = "#62162E"
color13     = "#7C1C3B"
color6      = "#566F6E"
color14     = "#659A91"
color7      = "#B99F8D"
color15     = "#F3DCC6"
---------------------------------------
-- Prompt config
---------------------------------------
myXPConfig :: XPConfig
myXPConfig = defaultXPConfig
    { font              = myFont
    , bgColor           = background
    , fgColor           = foreground
    , fgHLight          = background
    , bgHLight          = foreground
    , borderColor       = foreground
    , promptBorderWidth = 0
    , position          = Top
    , height            = 15
    , defaultText       = []
    }
---------------------------------------
-- Gridselect config
---------------------------------------
-- GridSelect color scheme
myColorizer :: Window -> Bool -> X (String, String)
myColorizer = colorRangeFromClassName
    (0x00,0x00,0x00) --lowest inactive bg
    (0x1C,0x1C,0x1C) --highest inactive bg
    (0x44,0xAA,0xCC) --active bg
    (0xBB,0xBB,0xBB) --inactive fg
    (0x00,0x00,0x00) --active fg

-- GridSelect theme
myGSConfig :: t -> GSConfig Window
myGSConfig colorizer = (buildDefaultGSConfig myColorizer)
    { gs_cellheight  = 24
    , gs_cellwidth   = 200
    , gs_cellpadding = 10
    , gs_font        = myFont
    }

spawnGSConfig :: HasColorizer a => GSConfig a
spawnGSConfig = defaultGSConfig
    { gs_cellheight  = 24
    , gs_cellwidth   = 140
    , gs_cellpadding = 10
    , gs_font        = myFont
    }

---------------------------------------
-- Key bindings
---------------------------------------
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $
    -- Spawn a terminal
    [ ((modm .|. shiftMask	, xK_Return), spawn $ XMonad.terminal conf)
    -- Spawn stuff
    , ((modm			, xK_i     ), spawn "termite --title=irssi --exec=irssi" )
    , ((modm			, xK_a     ), spawn "termite --title=alsamixer --exec=alsamixer" )
    , ((modm			, xK_m     ), spawn "termite --title=ncmpcpp --exec=ncmpcpp" )
    -- Spawn keybindings
    , ((modm			, xK_d     ), spawn "dwb")
    , ((modm .|. shiftMask , xK_d  ), spawn "xclip -o | xargs dwb")
    , ((modm			, xK_e     ), spawn "termite --title=ranger --exec=ranger" )
    -- Prompts
    , ((modm			, xK_o     ), shellPrompt myXPConfig )
    , ((modm			, xK_p     ), manPrompt myXPConfig )
    , ((modm			, xK_w     ), spawn "uri=$(/home/tlw/scripts/surfraw-dmenu.sh) && dwb $uri" )
    -- gridSelect
    , ((modm            , xK_s     ), goToSelected $ myGSConfig myColorizer )
    , ((modm .|. shiftMask , xK_s  ), spawnSelected spawnGSConfig ["~/diablo/rund2a.sh","~/diablo/rund2b.sh","~/diablo/rund2c.sh","~/diablo/rund2d.sh","~/diablo/rund2e.sh"] )
    -- Desktop notifications
    , ((modm			, xK_c     ), spawn "/home/tlw/scripts/notify_cal.sh" )
    , ((modm .|. shiftMask	, xK_o     ), spawn "/home/tlw/scripts/notify_cow.sh" )
    -- Control LCD brightness
    , ((modm .|. shiftMask	, xK_plus  ), spawn "xbacklight -inc 10" )
    , ((modm .|. shiftMask	, xK_minus ), spawn "xbacklight -inc -10" )
    -- Control mpd
    -- Play / pause song in mpd
    , ((modm .|. shiftMask	, xK_p     ), spawn "ncmpcpp toggle")
    , ((modm			, xK_Left  ), spawn "ncmpcpp prev")
    , ((modm			, xK_Right ), spawn "ncmpcpp next")
    , ((0			, xF86XK_AudioLowerVolume), spawn "amixer -q set Master 2dB- && /home/tlw/scripts/notify_vol.sh")
    , ((0			, xF86XK_AudioRaiseVolume), spawn "amixer -q set Master 2dB+ && /home/tlw/scripts/notify_vol.sh")
    , ((0			, xF86XK_AudioMute), spawn "amixer -q set Master toggle")
    -- Move to next non-empty workspace
    , ((modm			, xK_n     ), moveTo Next NonEmptyWS )
    -- Move to next empty workspace
    , ((modm .|. shiftMask	, xK_n     ), moveTo Next EmptyWS )
    -- Close focused window
    , ((modm .|. shiftMask	, xK_w     ), kill)
    -- Rotate through the available layout algorithms
    , ((modm			, xK_space ), sendMessage NextLayout)
    -- Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask	, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Move focus to the next window
    , ((modm			, xK_Tab   ), windows W.focusDown)
    -- Move focus to the next window
    , ((modm			, xK_j     ), windows W.focusDown)
    -- Move focus to the previous window
    , ((modm			, xK_k     ), windows W.focusUp  )
    -- Move focus to the master window
    --, ((modm			, xK_m     ), windows W.focusMaster  )
    -- Swap the focused window and the master window
    , ((modm			, xK_Return), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modm .|. shiftMask	, xK_j     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask	, xK_k     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modm			, xK_h     ), sendMessage Shrink)
    -- Expand the master area
    , ((modm			, xK_l     ), sendMessage Expand)
    -- Push window back into tiling
    , ((modm			, xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
    , ((modm			, xK_comma ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modm			, xK_period), sendMessage (IncMasterN (-1)))
    -- Quit xmonad
    , ((modm .|. shiftMask	, xK_q     ), io (exitWith ExitSuccess))
    -- Restart xmonad
    , ((modm			, xK_q     ), spawn "killall bar; killall conky; cd ~/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux; xmonad --restart")
    ]
    ++
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_6]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    {-
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    -}
---------------------------------------
-- Workspaces
---------------------------------------
-- Workspace definitions
myWorkspaces		= ["i","ii","iii","iv","v","vi"]
-- Declare workspace rules
myLayout = onWorkspace ( myWorkspaces !! 0 ) ( avoidStruts tiled ||| fullScreen )
         $ avoidStruts ( tiled ||| spiraled ) ||| fullScreen
    where
        tiled		= spacing 10 $ ResizableTall nmaster delta ratio []
        fullScreen	= noBorders ( fullscreenFull Full )
        spiraled	= spiral (ratio)
        -- Default number of windows in master pane
        nmaster		= 1
        -- Percent of the screen to increment when resizing
        delta		= 5/100
        -- Default proportion of the screen taken up by main pane
        ratio		= toRational (2/(1 + sqrt 5 :: Double))
---------------------------------------
-- Application specific rules
---------------------------------------
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

myManageHook :: Query (Endo WindowSet)
myManageHook = composeAll . concat $
    [ [ resource    =? "irssi"     --> doShift ( myWorkspaces !! 1 ) ]
    , [ resource    =? "weechat"   --> doShift ( myWorkspaces !! 1 ) ]
    , [ resource    =? "ncmpcpp"   --> doShift ( myWorkspaces !! 3 ) ]
    , [ resource    =? "alsamixer" --> doShift ( myWorkspaces !! 3 ) ]
    , [ className   =? c           --> doCenterFloat | c <- floats ]
    , [ resource    =? r           --> doIgnore      | r <- ignore ]
    , [ isDialog                   --> doCenterFloat ]
    , [ isFullscreen               --> doFullFloat ]
--    , [ isFullscreen               --> ( doF W.focusDown <+> doFullFloat ) ]
    ]
  where floats = ["wine", "vlc", "feh", "meh", "imagej", "fiji"]
        ignore = ["bar"]
---------------------------------------
-- Status bar
---------------------------------------
myXmonadBar	= "/usr/bin/bar -p"
myBottomBar	= "conky -c /home/tlw/.xmonad/statusbar_conkyrc | /usr/bin/bar -bp"

myLogHook :: Handle -> X ()
myLogHook h = dynamicLogWithPP defaultPP
    { ppExtras = [ lMpd, lDate ]
    , ppCurrent         = wrap "\\b0\\u8" "\\br\\ur" . pad
    --, ppVisible         = wrap "\\f3" "\\fr" . pad
    , ppHidden          = wrap "\\b0" "\\br" . pad
    , ppHiddenNoWindows = wrap "\\f4" "\\fr" . pad
    , ppUrgent          = wrap "\\f1" "\\fr" . pad
    , ppWsSep           = ""
    , ppSep             = ""
    , ppLayout          = wrap "\\f4" "\\fr" . pad
    , ppTitle           = wrap "" "" . shorten 90 . pad
    , ppOrder           = \(ws:_:t:xs) -> [wrap "\\l" "" $ t, wrap "\\c" "\\r" $ ws] ++ xs
    , ppOutput          = hPutStrLn h
    }
  where
    lDate = padL $ date "%H:%M"
    lMpd = wrapL "\\f4" "\\fr" . padL $ logCmd "mpc current"
