import XMonad
import XMonad.Config.Desktop
import XMonad.Config.Bluetile
    
import qualified XMonad.StackSet as W -- to shift and float windows
import qualified Data.Map as M
import Data.Monoid

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers -- isFullscreen, doFullFloat
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.RestoreMinimized
    
import XMonad.Layout.Dishes
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import XMonad.Layout.MultiToggle
import XMonad.Layout.Reflect
import XMonad.Layout.NoBorders
import XMonad.Layout.Spiral
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.Minimize
import XMonad.Layout.FixedColumn
import XMonad.Layout.BoringWindows
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.BorderResize
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.DraggingVisualizer
--import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.WindowSwitcherDecoration
    
import XMonad.Actions.NoBorders
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Replace
    
import System.Exit

import Text.Regex.PCRE

import Network.BSD
    
doNoBorders :: ManageHook -- Query (Endo WindowSet) -> ReaderT Window X (Endo Windowset)
doNoBorders = do
  w <- ask
  liftX $ withDisplay $ \d -> io $ setWindowBorderWidth d w 0
  doF . W.float w . snd =<< liftX (floatLocation w)

myManageHook = composeAll . concat $
    [ [ className =? c --> doFloat | c <- myFloats]
    , [ title =? t --> doFloat | t <- myFloats]
    , [ resource  =? "desktop_window" --> doIgnore 
      , isFullscreen --> doFullFloat
      , workspaceByPos
      , positionStoreManageHook (Just defaultThemeWithButtons)
      , manageDocks
      , title =? "*Remember*" --> doFloat
      , stringProperty "WM_ICON_NAME" =? "*Remember*" --> doFloat
      , (fmap (=~ "^Copying.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "^Moving.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "^Transferring.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "^Progress.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "^Drive Calculator: DCbase\\.dcd.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "Drive.*Calculator") $ stringProperty "WM_CLASS") --> doFloat
      , (fmap (=~ "Osmos") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ ".*plasmoidviewer.*") $ stringProperty "WM_COMMAND") --> doFloat
      , (fmap (=~ ".*[Zz]enity.*") $ stringProperty "WM_CLASS") --> doFloat
      , (fmap (=~ ".*Steam\\.exe.*") $ stringProperty "WM_CLASS") --> doFloat
      , (fmap (=~ ".*Wine.*") $ stringProperty "WM_CLASS") --> doFloat
      , (fmap (=~ ".*[Pp]lasma.*") $ stringProperty "WM_CLASS") --> doIgnore
      , (fmap (=~ "SparkleShare") $ stringProperty "WM_CLASS") --> doFloat
      ]
    ] where

        myFloats = ["Knetworkmanager"
                   ,"knetworkmanager"
                   ,"gwenview"
                   ,"Gwenview"
                   ,"vlc"
                   ,"Vlc"
                   ,"VLC (XVideo output)"
                   ,"VLC (GLX output)"
                   ,"Kaffeine"
                   ,"xine"
                   ,"Dragon"
                   ,"Kmix"
                   ,"VirtualBox"
                   ,"XScreensaver"
                   ,"xscreensaver"
                   ,"openttd"
                   ,"Pidgin"
                   ,"Savage 2 - A Tortured Soul"
                   ,"native_client"
                   ,"audacious"
                   ,"Blender:Render"
                   ,"<unknown>"
                   ]

myLayout =
    avoidStruts $
    minimize $
    maximize $
    boringWindows $
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $
    --spacing 2 $
    reflectHoriz $
    smartBorders $
    fixed ||| (spiral (6/7)) ||| full ||| (reflectHoriz $ mosaicalt)
  where
    mosaicalt = MosaicAlt M.empty
    dishes = Dishes 2 (1/6)
    tiled  = Tall 1 (3/100) (1/2)
    full = Full
    fixed = FixedColumn 1 20 120 10
    tilingDeco l = windowSwitcherDecorationWithButtons shrinkText defaultThemeWithButtons (draggingVisualizer l)
    floatingDeco l = buttonDeco shrinkText defaultThemeWithButtons l

myTabTheme = defaultTheme { activeColor = darkYellow
                          , inactiveColor = gray
                          --, urgentColor = yellow
                          , activeBorderColor = intenseYellow
                          , inactiveBorderColor = backgroundBlack
                          --, urgentBorderColor = yellow
                          , activeTextColor = backgroundBlack
                          , inactiveTextColor = backgroundBlack
                          --, urgentTextColor = backgroundBlack
                          , fontName = tabFont
                          --, decoWidth =
                          , decoHeight = 16 }

myWorkspaces = ["1","2","3","4","5","6","7","8","9","10","11","12"]

myNormalBorderColor  = intenseBlack -- "#d4d7d0"
myFocusedBorderColor = intenseYellow
myBorderWidth = 2

-- TODO extract colors from .Xdefaults
foregroundWhite = "#ffffff"
backgroundBlack = "#000000"

darkYellow = "#D37C18"

black = "#3A3A3A"
~red = "#F92673"
green = "#9AD22A"
yellow = "#FD951D"
blue = "#66DDEF"
magenta = "#9B6DFE"
cyan = "#5E7175"
white = "#DFDFDA"

intenseBlack = "#3c3c3c"
intenseRed = "#F96098"
intenseGreen = "#A7CF58"
intenseYellow = "#ff8c00"
intenseBlue = "#88E3EF"
intenseMagenta = "#B08BFF"
intenseCyan = "#769CA3"
intenseWhite = "#ffffff"

gray = "#525252"

dzenFont h | h == "capcom" = "dejavu\\ sans:size=10"
           | h == "spirit" = "dejavu\\ sans:size=8"
dzenFontMono h | h == "capcom" = "dejavu\\ sans\\ mono:size=10"
               | h == "spirit" = "dejavu\\ sans\\ mono:size=8"

tabFont = "-*-profont-*-*-*-*-14-*-*-*-*-*-*-*"
    
myPP h = defaultPP
         { ppCurrent = wrap ("^fg(" ++ red ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)[") "]^p(2)^fg()^bg()"
         , ppVisible = wrap ("^fg(" ++ blue ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)[") "]^p(2)^fg()^bg()"
         , ppHidden = wrap ("^fg(" ++ green ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)") "^p(2)^fg()^bg()"
         , ppHiddenNoWindows = wrap ("^fg(" ++ white ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)") "^p(2)^fg()^bg()"
         , ppSep     = " ^fg(grey60)^r(1x12)^fg() "
         , ppLayout  = dzenColor white "" . (\x -> case x of
                                                     "Minimize Full" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-full.xbm)"
                                                     "Minimize Maximize ReflectX Full" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-full.xbm)"
                                                     "Minimize Maximize ReflectY Full" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-full.xbm)"
                                                     "Minimize Maximize ReflectX ReflectY Full" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-full.xbm)"
                                                     "Minimize Maximize ReflectY ReflectX Full" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-full.xbm)"
                                                     "Minimize Maximize ReflectX ReflectX Full" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-full.xbm)"
                                                     "Minimize Maximize ReflectX ReflectY ReflectX Full" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-full.xbm)"
                                                     "Minimize Maximize Spiral" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-spiral.xbm)"
                                                     "Minimize Maximize ReflectX Spiral" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-spiral-horiz.xbm)"
                                                     "Minimize Maximize ReflectY Spiral" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-spiral-vert.xbm)"
                                                     "Minimize Maximize ReflectX ReflectY Spiral" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-spiral-horiz-vert.xbm)"
                                                     "Minimize Maximize ReflectX ReflectX Spiral" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-spiral.xbm)"
                                                     "Minimize Maximize ReflectY ReflectX Spiral" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-spiral-horiz-vert.xbm)"
                                                     "Minimize Maximize ReflectX ReflectY ReflectX Spiral" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-spiral-vert.xbm)"
                                                     "Minimize Maximize MosaicAlt" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mosaic.xbm)"
                                                     "Minimize Maximize ReflectX MosaicAlt" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mosaic.xbm)"
                                                     "Minimize Maximize ReflectY MosaicAlt" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mosaic.xbm)"
                                                     "Minimize Maximize ReflectX ReflectY MosaicAlt" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mosaic.xbm)"
                                                     "Minimize Maximize ReflectY ReflectX MosaicAlt" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mosaic.xbm)"
                                                     "Minimize Maximize ReflectX ReflectX MosaicAlt" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mosaic.xbm)"
                                                     "Minimize Maximize ReflectX ReflectY ReflectX MosaicAlt" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mosaic.xbm)"
                                                     "Minimize Maximize FixedColumn" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-tall-right.xbm)"
                                                     "Minimize Maximize ReflectX FixedColumn" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-tall-left.xbm)"
                                                     "Minimize Maximize ReflectY FixedColumn" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-tall-right.xbm)"
                                                     "Minimize Maximize ReflectX ReflectY FixedColumn" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-tall-left.xbm)"
                                                     "Minimize Maximize ReflectY ReflectX FixedColumn" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-tall-left.xbm)"
                                                     "Minimize Maximize ReflectX ReflectX FixedColumn" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-tall-right.xbm)"
                                                     "Minimize Maximize ReflectX ReflectY ReflectX FixedColumn" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-tall-right.xbm)"
                                                     "Minimize Maximize Dishes 2 (1 % 6)" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mirror-bottom.xbm)"
                                                     "Minimize Maximize ReflectX Dishes 2 (1 % 6)" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mirror-bottom.xbm)"
                                                     "Minimize Maximize ReflectY Dishes 2 (1 % 6)" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mirror-top.xbm)"
                                                     "Minimize Maximize ReflectX ReflectY Dishes 2 (1 % 6)" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mirror-top.xbm)"
                                                     "Minimize Maximize ReflectY ReflectX Dishes 2 (1 % 6)" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mirror-top.xbm)"
                                                     "Minimize Maximize ReflectX ReflectX Dishes 2 (1 % 6)" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mirror-bottom.xbm)"
                                                     "Minimize Maximize ReflectX ReflectY ReflectX Dishes 2 (1 % 6)" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mirror-top.xbm)"
                                                     _ -> pad x
                                                 )
         , ppUrgent  = dzenColor backgroundBlack yellow . wrap "!" "!"
         , ppTitle   = \_ -> ""
         , ppSort    = getSortByIndex >>= \f -> return $ f . namedScratchpadFilterOutWorkspace
         --, ppExtras  = [logMail]
         , ppOutput  = hPutStrLn h -- . (++) " "
         }

myScratchpads = [ NS "Volume Control" "pavucontrol" (title =? "Volume Control") (customFloating $ W.RationalRect (1/3) (1/6) (1/3) (2/3))
                -- , NS "Agenda" "xemacs -e \"(org-agenda-list)\"" (title =? "*Org Agenda*") (customFloating $ W.RationalRect (2/3) (2/3) (1/3) (1/3))
                ]
    where
      role = stringProperty "WM_WINDOW_ROLE"

--main = replace >> xmonad bluetileConfig


{-bluetileMouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
bluetileMouseBindings (XConfig {XMonad.modMask = modMask'}) = M.fromList $
    -- mod-button1 %! Move a floated window by dragging
    [ ((modMask', button1), (\w -> isFloating w >>= \isF -> when (isF) $
                                focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    -- mod-button2 %! Switch to next and first layout
    , ((modMask', button2), (\_ -> sendMessage NextLayout))
    , ((modMask' .|. shiftMask, button2), (\_ -> sendMessage $ JumpToLayout "Floating"))
    -- mod-button3 %! Resize a floated window by dragging
    , ((modMask', button3), (\w -> isFloating w >>= \isF -> when (isF) $
                                focus w >> mouseResizeWindow w >> windows W.shiftMaster))
    ]-}
             
main = do
    hn <- getHostName
    h <- case hn of
           "spirit" -> do
               h2 <- spawnPipe $
                       "dzen-launcher.pl -w 300 -y 786 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                       ++ "-w 212 -x 260 -y 786 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "                      
                       ++ "-l \\\"^fg\\(" ++ yellow ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/cpu.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o --- "
                       ++ "-l \\\"^fg\\(" ++ magenta ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/mem.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ magenta ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5 --- "
                       ++ "-w 200 -x 1166 -y 786 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta r -fn \\\"" ++ dzenFontMono hn ++ "\\\" --- "
                       ++ "-w 150 -x 472 -y 786 -fg \\\"" ++ green ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                       ++ "-l \\\"^fg\\(" ++ green ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/bat_full_02.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ green ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5 --- "
                       ++ "-w 555 -x 622 -y 786 -fg \\\"" ++ red ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                       ++ "-l \\\"^fg\\(" ++ red ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/wifi_01.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ red ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5"
               spawn "sleep 20; trayer --align left --edge bottom --SetDockType true --SetPartialStrut true --expand true --width 300 --height 18 --transparent true --tint 0x000000 --widthtype pixel --margin 880"
               return h2    
           "capcom" ->
               spawnPipe $
                       "dzen-launcher.pl -w 560 -y 1200 -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ backgroundBlack ++ "\\\" -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                       ++ "-w 220 -x 341 -y 1200 -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ backgroundBlack ++ "\\\" -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                       ++ "-l \\\"^fg\\(" ++ yellow ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/cpu.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o --- "
                       ++ "-l \\\"^fg\\(" ++ magenta ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/mem.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ magenta ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5 --- "
                       ++ "-w 223 -x 1697 -y 1200 -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ backgroundBlack ++ "\\\" -p -ta r -fn \\\"" ++ dzenFontMono hn ++ "\\\""
           otherwise -> spawnPipe "dzen2"

    replace
    xmonad $ desktopConfig
        { terminal = "konsole"
        , modMask = mod4Mask -- use the Windows button as mod
        , logHook =
            currentWorkspaceOnTop >>
            ewmhDesktopsLogHook >>
            setWMName "LG3D" >>
            (dynamicLogWithPP $ myPP h)
        , manageHook =
            namedScratchpadManageHook myScratchpads <+>
            manageHook desktopConfig <+>
            myManageHook
        , layoutHook = desktopLayoutModifiers $ myLayout
        , workspaces = myWorkspaces
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth = myBorderWidth
        , focusFollowsMouse = True
        , handleEventHook = ewmhDesktopsEventHook
                                `mappend` fullscreenEventHook
                                `mappend` restoreMinimizedEventHook
                                --`mappend` serverModeEventHook' bluetileCommands
                                --`mappend` positionStoreEventHook
        --, mouseBindings = bluetileMouseBindings
        }
        `removeKeys`
        [ (mod4Mask .|. shiftMask, xK_j)
        , (mod4Mask .|. shiftMask, xK_k)
        , (mod4Mask .|. shiftMask, xK_e)
        , (mod4Mask .|. shiftMask, xK_w)
        , (mod4Mask .|. shiftMask, xK_o)
        , (mod4Mask .|. shiftMask, xK_f)
        , (mod4Mask, xK_j)
        , (mod4Mask, xK_k)
        , (mod4Mask, xK_e)
        , (mod4Mask, xK_w)
        , (mod4Mask, xK_o)
        , (mod4Mask, xK_f)
        ]
        `additionalKeys`
        ([ ((mod4Mask, xK_F1),
            spawn "xemacs")
         , ((mod4Mask, xK_F2),
            spawn "urxvt -title '*Remember*' -e bash -c \"/home/lazor/bin/remember-launcher.sh '-e (make-remember-frame-terminal)'\"")
         , ((mod4Mask, xK_F3),
            spawn "conkeror")
         , ((mod4Mask, xK_F4),
            spawn "xwanderlust")
         , ((mod4Mask, xK_F5),
            spawn "xemacs -e \"(org-agenda-list)\"")
         , ((mod4Mask, xK_F6),
            namedScratchpadAction myScratchpads "Volume Control")
         , ((mod4Mask, xK_F7),
            spawn "amarok")

         , ((mod4Mask, xK_Return),
            spawn "krunner")


         , ((mod4Mask .|. shiftMask, xK_q),
            -- spawn "touch /home/lazor/.xmonad/dosystrayfix"
            spawn "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:-1 int32:1")
         , ((mod4Mask, xK_q), 
            spawn "ps ax | grep dzen | awk '{print $1}' | xargs kill"
            -- >> spawn "killall -9 trayer"
            -- >> spawn "/bin/rm /home/lazor/.xmonad/dosystrayfix"
            >> spawn "xmonad --recompile; xmonad --restart")

         , ((mod4Mask, xK_r), sendMessage Expand >> withFocused (sendMessage . expandWindowAlt)) -- >> sendMessage Taller >> sendMessage Expand)
         , ((mod4Mask, xK_s), sendMessage Shrink >> withFocused (sendMessage . shrinkWindowAlt)) -- >> sendMessage Wider >> sendMessage Shrink)
         , ((mod4Mask .|. shiftMask, xK_d), withFocused (sendMessage . tallWindowAlt))
         , ((mod4Mask .|. shiftMask, xK_s), withFocused (sendMessage . wideWindowAlt))
         , ((mod4Mask .|. controlMask, xK_space), sendMessage resetAlt)

         , ((mod4Mask, xK_x), sendMessage $ Toggle REFLECTX)
         , ((mod4Mask, xK_y), sendMessage $ Toggle REFLECTY)
         , ((mod4Mask, xK_b), withFocused toggleBorder)

         , ((mod4Mask, xK_n), windows $ (\s ->
             let ws = takeWhile (/=W.currentTag s) myWorkspaces
                 prev_workspace = case ws of
                                     [] -> last myWorkspaces
                                     otherwise -> last ws
             in W.greedyView prev_workspace s))
         , ((mod4Mask, xK_i), windows $ (\s ->
             let ws = reverse $ takeWhile (/=W.currentTag s) $ reverse myWorkspaces
                 next_workspace = case ws of
                                     [] -> head myWorkspaces
                                     otherwise -> head ws
             in W.greedyView next_workspace s))

         , ((mod4Mask, xK_u), windows W.focusUp)
         , ((mod4Mask, xK_e), windows W.focusDown)
         , ((mod4Mask .|. shiftMask, xK_u), windows W.swapUp)
         , ((mod4Mask .|. shiftMask, xK_e), windows W.swapDown)

         , ((mod4Mask, xK_c), cycleRecentWindows [xK_Super_R] xK_c xK_v)
         , ((mod4Mask, xK_v), cycleRecentWindows [xK_Super_R] xK_v xK_c)

         , ((mod4Mask, xK_m), withFocused minimizeWindow)
         , ((mod4Mask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
                   
         , ((mod4Mask, xK_g), toggleWS)
           
         ] ++

         [ ((m .|. mod4Mask, k), windows $ f i)
             | (i, k) <- zip myWorkspaces ([xK_1 .. xK_6] ++ [xK_7 .. xK_9] ++ [xK_0,xK_equal,xK_bar])
             , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
         ] ++

         [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
             | (key, sc) <- zip [xK_l, xK_z] [0..]
             , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ] ++

         [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
             | (key, sc) <- zip [xK_w, xK_f] [0..]
             , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
         ]
         )
