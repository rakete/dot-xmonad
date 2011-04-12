{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import XMonad
import XMonad.Config.Desktop
    
import qualified XMonad.StackSet as S

import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromMaybe,isNothing)
import Data.List (find)

import Control.Monad
import Control.Arrow (second)

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
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.StackTile
--import XMonad.Layout.IndependentScreens
    
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
import System.Process
import System.Posix
    
import Text.Regex.PCRE

import Network.BSD
    
-- TODO extract colors from .Xdefaults
foregroundWhite = "#ffffff"
backgroundBlack = "#000000"

darkYellow = "#D37C18"

black = "#3A3A3A"
red = "#F92673"
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

dzenFont h | h == "capcom" = "dejavu\\ sans:size=12"
           | h == "spirit" = "dejavu\\ sans:size=8"
dzenFontMono h | h == "capcom" = "dejavu\\ sans\\ mono:size=12"
               | h == "spirit" = "dejavu\\ sans\\ mono:size=8"

tabFont = "-*-profont-*-*-*-*-14-*-*-*-*-*-*-*"        

-- doNoBorders :: ManageHook -- Query (Endo WindowSet) -> ReaderT Window X (Endo Windowset)
-- doNoBorders = do
--   w <- ask
--   liftX $ withDisplay $ \d -> io $ setWindowBorderWidth d w 0
--   doF . W.float w . snd =<< liftX (floatLocation w)

myNormalBorderColor  = intenseBlack -- "#d4d7d0"
myFocusedBorderColor = intenseYellow
myBorderWidth = 2
         
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
      , (fmap (=~ "^Deleting.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "^Document.Print.Status.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "^Drive Calculator: DCbase\\.dcd.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "Drive.*Calculator") $ stringProperty "WM_CLASS") --> doFloat
      , (fmap (=~ "Osmos") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ ".*plasmoidviewer.*") $ stringProperty "WM_COMMAND") --> doFloat
      , (fmap (=~ ".*[Zz]enity.*") $ stringProperty "WM_CLASS") --> doFloat
      , (fmap (=~ ".*Steam\\.exe.*") $ stringProperty "WM_CLASS") --> doFloat
      , (fmap (=~ ".*Wine.*") $ stringProperty "WM_CLASS") --> doFloat
      , (fmap (=~ ".*[Pp]lasma.*") $ stringProperty "WM_CLASS") --> doFloat
      , (fmap (=~ "SparkleShare") $ stringProperty "WM_CLASS") --> doFloat
      , (fmap (=~ "Extracting file...") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "Mumble.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "^Authorization.Dialog.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ "^ImageMagick:.*") $ stringProperty "WM_NAME") --> doFloat
      , (fmap (=~ ".*kinect.*") $ stringProperty "WM_NAME") --> doFloat
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

data TwoScreen sl ol a = TwoScreen ScreenId sl ol
                         deriving (Show, Read)

instance (LayoutClass sl a, LayoutClass ol a) => LayoutClass (TwoScreen (sl a) (ol a)) a where
    -- glance at the current screen to determine which layout to run
    runLayout (S.Workspace i (TwoScreen s sl ol) ms) r = do
        mts <- findScreenByTag i
        case liftM ((== s) . S.screen) mts of
            Just True -> fmap (second . fmap $ \nsl -> TwoScreen s nsl ol)
                       $ runLayout (S.Workspace i sl ms) r
            _ -> fmap (second . fmap $ \nol -> TwoScreen s sl nol)
               $ runLayout (S.Workspace i ol ms) r

    -- route messages to both sub-layouts (ick)
    handleMessage l@(TwoScreen s sl ol) m = do
        msl <- handleMessage sl m
        mol <- handleMessage ol m
        return $ if isNothing msl && isNothing mol
         then Nothing
         else Just $ TwoScreen s (fromMaybe sl msl) (fromMaybe ol mol)

    description (TwoScreen s sl ol) = "TwoScreen " ++ (description sl)

findScreenByTag i =
    gets (S.screens . windowset) >>= return . find ((== i) . (S.tag . S.workspace))

mkTwoScreen :: (LayoutClass sl a, LayoutClass ol a) => Int -> (sl a) -> (ol a) -> TwoScreen (sl a) (ol a) a
mkTwoScreen i = TwoScreen (S i) 

myLayout =
    --avoidStruts $
    --minimize $
    --maximize $
    --boringWindows $
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $
    --spacing 2 $
    reflectHoriz $
    smartBorders $
    (mkTwoScreen 0 resizeable full) ||| (mkTwoScreen 0 leftspiral full) ||| (mkTwoScreen 0 downspiral full) 
  where
    mosaicalt = MosaicAlt M.empty
    dishes = Dishes 2 (1/6)
    tiled  = Tall 1 (3/100) (1/2)
    full = Full
    fixed = FixedColumn 1 5 120 10
    stack = StackTile 3 (3/100) (1/2)
    downspiral = spiralWithDir South XMonad.Layout.Spiral.CW (6/7)
    leftspiral = spiralWithDir East XMonad.Layout.Spiral.CW (6/7)
    resizeable = ResizableTall 1 (3/100) (1/2) []

myPP (Just h) = defaultPP
         { ppCurrent = wrap ("^fg(" ++ red ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)[") "]^p(2)^fg()^bg()"
         , ppVisible = wrap ("^fg(" ++ white ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)[") "]^p(2)^fg()^bg()"
         , ppHidden = wrap ("^fg(" ++ green ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)") "^p(2)^fg()^bg()"
         , ppHiddenNoWindows = wrap ("^fg(" ++ white ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)") "^p(2)^fg()^bg()"
         , ppSep     = " ^fg(grey60)^r(1x12)^fg() "
         , ppLayout  = dzenColor white "" . (\x ->
                                                 let tokens = words x
                                                     xs = filter (=="ReflectX") tokens
                                                     ys = filter (=="ReflectY") tokens
                                                     name = head $ filter (\t -> (all (/=t) ["Minimize","Maximize","ReflectX","ReflectY", "TwoScreen"])) tokens
                                                 in case name of
                                                      "Full" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-full.xbm)"
                                                      "Spiral" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-spiral.xbm)"
                                                      "FixedColumn" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-tall-right.xbm)"
                                                      "ResizableTall" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mosaic.xbm)"
                                                      otherwise -> pad name
                                            )
         , ppUrgent  = dzenColor backgroundBlack yellow . wrap "!" "!"
         , ppTitle   = \_ -> ""
         , ppSort    = getSortByIndex >>= \f -> return $ f . namedScratchpadFilterOutWorkspace
         --, ppExtras  = [logMail]
         , ppOutput  = hPutStrLn h -- . (++) " "
         }
myPP Nothing = defaultPP
                
main = do
    hn <- getHostName
    h <- case hn of
           "spirit" -> do
                   -- BEWARE OF THE UGLY HACK!
                   spawn "/bin/bash -e /home/lazor/bin/resolution.sh > /tmp/resolution"
                   sleep 1
                   resolution <- readFile "/tmp/resolution"
                   putStr resolution
                   h2 <- case resolution of
                           "1920x1080\n" -> do
                                           h3 <- spawnPipe $ "dzen-launcher.pl -w 300 -y 1080 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                                                         ++ "-w 212 -x 260 -y 1080 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "                      
                                                         ++ "-l \\\"^fg\\(" ++ yellow ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/cpu.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o --- "
                                                         ++ "-l \\\"^fg\\(" ++ magenta ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/mem.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ magenta ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5 --- "
                                                         ++ "-w 200 -x 1720 -y 1080 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta r -fn \\\"" ++ dzenFontMono hn ++ "\\\" --- "
                                                         ++ "-w 150 -x 472 -y 1080 -fg \\\"" ++ green ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                                                         ++ "-l \\\"^fg\\(" ++ green ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/bat_full_02.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ green ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5 --- "
                                                         ++ "-w 855 -x 610 -y 1080 -fg \\\"" ++ red ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                                                         ++ "-l \\\"^fg\\(" ++ red ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/wifi_01.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ red ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5"
                                           spawn "sleep 5; trayer --align left --edge bottom --SetDockType true --SetPartialStrut true --expand true --width 300 --height 18 --transparent true --tint 0x000000 --widthtype pixel --margin 1434"
                                           return h3
                           otherwise -> do
                             h3 <- spawnPipe $ "dzen-launcher.pl -w 300 -y 786 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                                           ++ "-w 212 -x 260 -y 786 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "                      
                                           ++ "-l \\\"^fg\\(" ++ yellow ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/cpu.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o --- "
                                           ++ "-l \\\"^fg\\(" ++ magenta ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/mem.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ magenta ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5 --- "
                                           ++ "-w 200 -x 1166 -y 786 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta r -fn \\\"" ++ dzenFontMono hn ++ "\\\" --- "
                                           ++ "-w 150 -x 472 -y 786 -fg \\\"" ++ green ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                                           ++ "-l \\\"^fg\\(" ++ green ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/bat_full_02.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ green ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5 --- "
                                           ++ "-w 555 -x 610 -y 786 -fg \\\"" ++ red ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                                           ++ "-l \\\"^fg\\(" ++ red ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/wifi_01.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ red ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5"
                             spawn "sleep 5; trayer --align left --edge bottom --SetDockType true --SetPartialStrut true --expand true --width 300 --height 18 --transparent true --tint 0x000000 --widthtype pixel --margin 880"
                             return h3
                   return $ Just h2    
           "capcom" -> do
               h2 <- spawnPipe $
                       "dzen-launcher.pl -w 345 -h 25 -y 0 -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ backgroundBlack ++ "\\\" -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
               return $ Just h2
           otherwise -> spawnPipe "dzen2" >>= return . Just                      

    replace
    xmonad $ applyMyKeyBindings $ desktopConfig
        { terminal = "konsole"
        , modMask = mod4Mask -- use the Windows button as mod
        , logHook =
            --currentWorkspaceOnTop >>
            ewmhDesktopsLogHook >>
            setWMName "LG3D" >>
            (dynamicLogWithPP $ myPP h)
        , manageHook =
            manageHook desktopConfig <+>
            myManageHook
        , layoutHook = desktopLayoutModifiers $ myLayout
        , workspaces = ["1","2","3","4","5","6","7","8","9","10","11","12"]
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth = myBorderWidth
        , focusFollowsMouse = True
        , handleEventHook = ewmhDesktopsEventHook
                                `mappend` fullscreenEventHook
                                `mappend` restoreMinimizedEventHook
                                `mappend` positionStoreEventHook
        , startupHook = do
            startupHook desktopConfig
            switchScreenToDesktop 1 "11"
        }

switchScreenToDesktop sc d = do
    mi <- screenWorkspace sc
    whenJust mi $ (\i ->
                    windows (\s ->
                             let c = S.currentTag s
                             in S.view c $ S.greedyView d $ S.view i s))

shiftByFollow :: Int -> X ()
shiftByFollow d = do
  i <- wsBy d
  windows $ S.shift i
  windows $ S.view i


wsBy :: Int -> X (WorkspaceId)
wsBy = findWorkspace getSortByIndex Next AnyWS

       
applyMyKeyBindings conf =
    conf
    `removeKeys`
    [ (mod4Mask .|. shiftMask, xK_j)
    , (mod4Mask .|. shiftMask, xK_k)
    , (mod4Mask .|. shiftMask, xK_e)
    , (mod4Mask .|. shiftMask, xK_w)
    , (mod4Mask .|. shiftMask, xK_o)
    , (mod4Mask .|. shiftMask, xK_f)
    , (mod4Mask .|. shiftMask, xK_q)
    , (mod4Mask .|. shiftMask, xK_r)
    , (mod4Mask, xK_j)
    , (mod4Mask, xK_k)
    , (mod4Mask, xK_e)
    , (mod4Mask, xK_w)
    , (mod4Mask, xK_o)
    , (mod4Mask, xK_f)
    , (mod4Mask, xK_q)
    , (mod4Mask, xK_l)
    ]
    `additionalKeys`
    ([ ((mod4Mask, xK_F1),
        spawn "kdesudo -u lazor /home/lazor/bin/xemacsclient.sh")
     , ((mod4Mask, xK_F2),
        spawn "urxvt -title '*Remember*' -e bash -c \"/home/lazor/bin/remember-launcher.sh '-e (make-remember-frame-terminal)'\"")
     , ((mod4Mask, xK_F3),
        spawn "xdg-open ./.local/share/applications/conkeror.desktop")
     , ((mod4Mask, xK_F4),
        spawn "kdesudo -u lazor /home/lazor/bin/xwanderlust.sh")
     , ((mod4Mask, xK_F5),
        spawn "/home/lazor/bin/xemacsclient.sh -e \"(org-agenda-list)\"")
     , ((mod4Mask, xK_F6),
        spawn "kdesudo -u lazor pavucontrol")
     , ((mod4Mask, xK_F7),
        spawn "kdesudo -u lazor amarok")
     , ((mod4Mask, xK_F8),
        spawn "kdesudo -u lazor konsole")
     , ((mod4Mask, xK_Return),
        spawn "kdesudo -u lazor krunner")
     , ((mod4Mask .|. shiftMask, xK_Escape),
        -- spawn "touch /home/lazor/.xmonad/dosystrayfix"
        spawn "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:-1 int32:1")
     , ((mod4Mask, xK_Escape), 
        spawn "ps ax | grep dzen | awk '{print $1}' | xargs kill"
        >> spawn "killall -9 trayer"
        -- >> spawn "/bin/rm /home/lazor/.xmonad/dosystrayfix"
        >> spawn "xmonad --recompile; xmonad --restart")





     , ((mod4Mask, xK_g), sendMessage $ IncMasterN 1)
     , ((mod4Mask .|. shiftMask, xK_g), sendMessage $ IncMasterN (-1))
     , ((mod4Mask, xK_r), sendMessage Expand) -- >> withFocused (sendMessage . expandWindowAlt))
     , ((mod4Mask, xK_s), sendMessage Shrink) -- >> withFocused (sendMessage . shrinkWindowAlt))
     , ((mod4Mask, xK_a), sendMessage MirrorExpand)
     , ((mod4Mask, xK_y), sendMessage MirrorShrink)
     --, ((mod4Mask .|. shiftMask, xK_r), sendMessage Taller >> withFocused (sendMessage . tallWindowAlt))
     --, ((mod4Mask .|. shiftMask, xK_s), sendMessage Wider >> withFocused (sendMessage . wideWindowAlt))

     , ((mod4Mask .|. controlMask, xK_space), sendMessage resetAlt)
     , ((mod4Mask, xK_x), sendMessage $ Toggle REFLECTX)
     , ((mod4Mask .|. shiftMask, xK_x), sendMessage $ Toggle REFLECTY)
     , ((mod4Mask, xK_b), withFocused toggleBorder)





       
     , ((mod4Mask, xK_n), do
          shiftByFollow (-1)
       )
     , ((mod4Mask, xK_i), do
          shiftByFollow 1
       )
     , ((mod4Mask, xK_u), windows S.swapUp)
     , ((mod4Mask, xK_e), windows S.swapDown)] ++

     -- [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
     --       | (key, sc) <- zip [xK_l, xK_z] [0..]
     -- , (f, m) <- [(S.view, 0), (S.shift, shiftMask)]] ++




     
     [ ((mod4Mask .|. shiftMask, xK_backslash), cycleRecentWindows [xK_Shift_R, xK_Shift_L] xK_backslash xK_Tab)
     , ((mod4Mask .|. shiftMask, xK_Tab), cycleRecentWindows [xK_Shift_R, xK_Shift_L] xK_Tab xK_backslash)
     , ((mod4Mask, xK_backslash), windows S.focusDown)
     , ((mod4Mask, xK_Tab), windows S.focusUp)

     --, ((mod4Mask, xK_c), cycleRecentWindows [xK_Super_R,xK_Super_L] xK_c xK_c)
     , ((mod4Mask, xK_m), withFocused minimizeWindow)
     , ((mod4Mask .|. shiftMask, xK_m), sendMessage RestoreNextMinimizedWin)
     , ((mod4Mask, xK_d), toggleWS)] ++
     [ ((m .|. mod4Mask, k), windows $ f i)
           | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_6] ++ [xK_7 .. xK_9] ++ [xK_0,xK_equal,xK_bar])
           , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)]] ++
     [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
           | (key, sc) <- zip [xK_w, xK_f] [0..]
     , (f, m) <- [(S.view, 0), (S.shift, shiftMask)]
     ])

-- data WrapType = Current | Visible | Hidden | HiddenNoWindows
    
-- independentWrap t s1 s2 id =
--     case t of
--       Current ->
--           if (fst $ unmarshall id) == 0 then
--               wrap s1 s2 (snd $ unmarshall id)
--           else
--               ""
--       Visible ->
--           snd $ unmarshall id
--       otherwise ->
--           id
    
