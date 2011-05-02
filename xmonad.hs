{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import XMonad
import XMonad.Config.Desktop
    
import qualified XMonad.StackSet as S

import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromMaybe,isNothing)
import Data.List (find,null)
import Data.IORef
    
import Control.Monad
import Control.Monad.Trans
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
import qualified XMonad.Layout.Spiral as SP
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
import XMonad.Layout.BoringWindows
    
import XMonad.Actions.NoBorders
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
    
import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Replace

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
    
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

doSideFloatWithBorder :: Side -> Rational -> ManageHook
doSideFloatWithBorder side border = doFloatDep move
  where
    move (S.RationalRect _ _ w h) = S.RationalRect cx cy w h
      where cx =      if side `elem` [SC,C ,NC] then (1-w)/2
                 else if side `elem` [SW,CW,NW] then 0 + border
                 else {- side `elem` [SE,CE,NE] -}   1-w - border
            cy =      if side `elem` [CE,C ,CW] then (1-h)/2
                 else if side `elem` [NE,NC,NW] then 0 + border
                 else {- side `elem` [SE,SC,SW] -}   1-h - border

toManageHook :: (X a) -> Query a
toManageHook = Query . lift
                 
myManageHook lastfocus = composeAll
    [ workspaceByPos
    , manageDocks
    , composeOne
      [ resource  =? "desktop_window" -?> doIgnore
      , isFullscreen -?> doFullFloat
      , isDialog -?> doCenterFloat

      , (fmap (=~ "^Copying.*") $ stringProperty "WM_NAME") -?> doSideFloatWithBorder SW 0.02
      , (fmap (=~ "^Moving.*") $ stringProperty "WM_NAME") -?> doSideFloatWithBorder SW 0.02
      , (fmap (=~ "^Transferring.*") $ stringProperty "WM_NAME") -?> doSideFloatWithBorder SW 0.02
      , (fmap (=~ "^Progress.*") $ stringProperty "WM_NAME") -?> doSideFloatWithBorder SW 0.02
      , (fmap (=~ "^Deleting.*") $ stringProperty "WM_NAME") -?> doSideFloatWithBorder SW 0.02
      , (fmap (=~ "^Document.Print.Status.*") $ stringProperty "WM_NAME") -?> doSideFloatWithBorder SW 0.02
      , (fmap (=~ "^Extracting file...") $ stringProperty "WM_NAME") -?> doSideFloatWithBorder SW 0.02

      , className =? "Xmessage" -?> doCenterFloat
      , (fmap (=~ "^Authorization.Dialog.*") $ stringProperty "WM_NAME") -?> doCenterFloat
      , (fmap (=~ "^Volume") $ stringProperty "WM_NAME") -?> doCenterFloat

      , (fmap (=~ ".*plasmoidviewer.*") $ stringProperty "WM_COMMAND") -?> doFloat
      , (fmap (=~ ".*[Pp]lasma.*") $ stringProperty "WM_CLASS") -?> doFloat

      , (fmap (=~ "^Drive Calculator: DCbase\\.dcd.*") $ stringProperty "WM_NAME") -?> doFloat
      , (fmap (=~ "Drive.*Calculator") $ stringProperty "WM_CLASS") -?> doFloat
      , (fmap (=~ "Osmos") $ stringProperty "WM_NAME") -?> doFloat
      , (fmap (=~ ".*[Zz]enity.*") $ stringProperty "WM_CLASS") -?> doFloat
      , (fmap (=~ ".*Steam\\.exe.*") $ stringProperty "WM_CLASS") -?> doFloat
      , (fmap (=~ ".*Wine.*") $ stringProperty "WM_CLASS") -?> doFloat
      , (fmap (=~ "SparkleShare") $ stringProperty "WM_CLASS") -?> doFloat
      , (fmap (=~ "Mumble.*") $ stringProperty "WM_NAME") -?> doFloat
      , (fmap (=~ "^ImageMagick:.*") $ stringProperty "WM_NAME") -?> doFloat
      , (fmap (=~ ".*kinect.*") $ stringProperty "WM_NAME") -?> doFloat
      , (fmap (=~ "^gimp.*tool") $ stringProperty "WM_WINDOW_ROLE") -?> doFloat
      , (fmap (=~ "^Adobe") $ stringProperty "WM_CLASS") -?> doFloat

      , stringProperty "WM_NAME" =? "*Remember*" -?> doCenterFloat

      , stringProperty "WM_NAME" =? "*Help*" -?> do
                                      toManageHook $ rememberFocus lastfocus
                                      insertPosition Below Newer
      , stringProperty "WM_NAME" =? "*compilation*" -?> insertPosition Below Older
      , stringProperty "WM_NAME" =? "*haskell*" -?> insertPosition Below Older
      , stringProperty "WM_NAME" =? "*ack*" -?> insertPosition Below Older

      , stringProperty "WM_WINDOW_ROLE" =? "Manager" -?> insertPosition Below Older

      , return True -?> do
          toManageHook $ rememberFocus lastfocus
          insertPosition Below Newer
      ]
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

data FixedSpiral = FixedSpiral SP.Direction SP.Rotation Rational
                     deriving ( Read, Show )
                
myLayout =
    --avoidStruts $
    minimize $
    maximize $
    --boringWindows $
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $
    --spacing 2 $
    reflectHoriz $
    smartBorders $
    boringAuto $
    (mkTwoScreen 0 resizeable full) ||| (mkTwoScreen 0 leftspiral full) ||| (mkTwoScreen 0 downspiral full) 
  where
    mosaicalt = MosaicAlt M.empty
    dishes = Dishes 2 (1/6)
    tiled  = Tall 1 (3/100) (1/2)
    full = Full
    fixed = FixedColumn 1 5 120 10
    stack = StackTile 3 (3/100) (1/2)
    downspiral = SP.spiralWithDir SP.South SP.CW (6/7)
    leftspiral = SP.spiralWithDir SP.East SP.CW (6/7)
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

type LastNumWindowsRef = IORef Int
type LastFocusRef = IORef (M.Map WorkspaceId Window)

main = do

  lastnumwin <- newIORef 0
  lastfocus <- newIORef M.empty

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
                       "dzen-launcher.pl -w 325 -h 25 -y 0 -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ backgroundBlack ++ "\\\" -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                 return $ Just h2
         otherwise -> spawnPipe "dzen2" >>= return . Just                      

  replace
  xmonad $ applyMyKeyBindings lastfocus $ desktopConfig
        { terminal = "konsole"
        , modMask = mod4Mask -- use the Windows button as mod
        , logHook =
            --currentWorkspaceOnTop >>
            ewmhDesktopsLogHook >>
            setWMName "LG3D" >>
            (dynamicLogWithPP $ myPP h) >>
            controlFocus lastnumwin lastfocus
        , manageHook =
            manageHook desktopConfig <+>
            myManageHook lastfocus
        , layoutHook = desktopLayoutModifiers $ myLayout
        , workspaces = ["1","2","3","4","5","6","7","8","9","10","11","12"]
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth = myBorderWidth
        , focusFollowsMouse = True
        , handleEventHook = ewmhDesktopsEventHook
                                `mappend` fullscreenEventHook
                                `mappend` restoreMinimizedEventHook
                                --`mappend` positionStoreEventHook
        , startupHook = do
            startupHook desktopConfig
            switchScreenToDesktop 1 "11"
        }

controlFocus :: LastNumWindowsRef -> LastFocusRef -> X ()
controlFocus lastnumwin lastfocus = do
  ws <- gets windowset
  case S.stack $ S.workspace $ S.current ws of
    (Just s) -> do
      let current_num_windows = (length $ S.up s) + (length $ S.down s)
      prev_num_windows <- io $ readIORef lastnumwin
      if (current_num_windows /= prev_num_windows)
       then do
        io $ writeIORef lastnumwin current_num_windows
        if (current_num_windows < prev_num_windows)
         then do
           mwin <- restoreFocus lastfocus
           maybe (return ()) (windows . S.focusWindow) mwin
         else return ()
       else return ()
    otherwise -> return ()
                 
rememberFocus :: LastFocusRef -> X ()
rememberFocus r = do
  ws <- gets windowset
  let w = S.workspace $ S.current ws
      wi = S.tag w
      ms = S.stack w
  case ms of
    (Just s) -> io $ modifyIORef r $ M.insert wi (S.focus s)
    otherwise -> return ()
                
focusUpRemember :: LastFocusRef -> X ()
focusUpRemember r = do
  rememberFocus r
  windows S.focusUp
                    
focusDownRemember :: LastFocusRef -> X ()
focusDownRemember r = do
  rememberFocus r
  windows S.focusDown

restoreFocus :: LastFocusRef -> X (Maybe Window)
restoreFocus r = do
  ws <- gets windowset
  let w = S.workspace $ S.current ws
      wi = S.tag w
      ms = S.stack w
  case ms of
    (Just s) -> do
            mwin <- io $ atomicModifyIORef r (\m ->
                                      case M.lookup wi m of
                                        (Just win) -> (M.delete wi m, Just win)
                                        otherwise -> (m,Nothing))
            return mwin
    otherwise -> return Nothing
          



switchScreenToDesktop sc d = do
    mi <- screenWorkspace sc
    whenJust mi $ (\i ->
                    windows (\s ->
                             let c = S.currentTag s
                             in S.view c $ S.greedyView d $ S.view i s))

myUpdatePointer = updatePointer (Relative 0.99 0.1)
       
applyMyKeyBindings lastFocusRef conf =
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
    , (mod4Mask .|. shiftMask, xK_c)
    , (mod4Mask .|. shiftMask, xK_q)
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
        spawn "pavucontrol")
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
        -- >> spawn "/bin/rm /home/lazor/.xmonad/dosystrayfix"
        spawn "xmonad --restart")
       
     -- , (("M4-\"), )
     -- , (("M4-<Tab>), )
     , ((mod4Mask .|. shiftMask, xK_backslash), cycleRecentWindows [xK_Shift_R, xK_Shift_L] xK_backslash xK_Tab)
     , ((mod4Mask .|. shiftMask, xK_Tab), cycleRecentWindows [xK_Shift_R, xK_Shift_L] xK_Tab xK_backslash)
     , ((mod4Mask, xK_backslash), do focusUpRemember lastFocusRef; myUpdatePointer)
     , ((mod4Mask, xK_Tab), do focusDownRemember lastFocusRef; myUpdatePointer)

       
     -- , (("M4-y"), )
     , ((mod4Mask, xK_y), toggleWS )
     -- , (("M4-w"), )
     , ((mod4Mask, xK_w), prevWS )
     , ((mod4Mask .|. shiftMask, xK_w), shiftToPrev >> prevWS )
     -- , (("M4-f"), )
     , ((mod4Mask, xK_f), nextWS )
     , ((mod4Mask .|. shiftMask, xK_f), shiftToNext >> nextWS )
     -- , (("M4-g"), )
     -- , ("M4-p", windows S.swapUp)
     , ((mod4Mask, xK_p), windows S.swapUp)
       
     -- , (("M4-a"), )
     , ((mod4Mask, xK_a), nextScreen)
     , ((mod4Mask .|. shiftMask, xK_a), shiftNextScreen)
     -- , (("M4-r"), )
     -- , (("M4-s"), )
     , ((mod4Mask, xK_r), sendMessage Expand)
     , ((mod4Mask, xK_s), sendMessage Shrink)
     , ((mod4Mask .|. shiftMask, xK_r), sendMessage MirrorExpand)
     , ((mod4Mask .|. shiftMask, xK_s), sendMessage MirrorShrink)
     -- , (("M4-t"), )
     -- , ("M4-d", windows S.swapDown)
     , ((mod4Mask, xK_d), windows S.swapDown)
       
     -- , (("M4-&"), )
     -- , (("M4-q"), )
     -- , (("M4-x"), )
     -- , (("M4-c"), )
     -- , (("M4-v"), toggleWS)
     , ((mod4Mask, xK_v), toggleWS)
     -- , (("M4-b"), )

       
     , ((mod4Mask .|. shiftMask, xK_q), kill)

     , ((mod4Mask, xK_c), withFocused minimizeWindow)
     , ((mod4Mask .|. shiftMask, xK_c), sendMessage RestoreNextMinimizedWin)

     , ((mod4Mask, xK_x), withFocused (sendMessage . maximizeRestore))
     
       
     , ((mod4Mask, xK_g), sendMessage $ IncMasterN 1)
     , ((mod4Mask .|. shiftMask, xK_g), sendMessage $ IncMasterN (-1))
     
     --, ((mod4Mask .|. shiftMask, xK_r), sendMessage Taller >> withFocused (sendMessage . tallWindowAlt))
     --, ((mod4Mask .|. shiftMask, xK_s), sendMessage Wider >> withFocused (sendMessage . wideWindowAlt))

     -- , ((mod4Mask .|. controlMask, xK_space), sendMessage resetAlt)
     -- , ((mod4Mask, xK_x), sendMessage $ Toggle REFLECTX)
     -- , ((mod4Mask .|. shiftMask, xK_x), sendMessage $ Toggle REFLECTY)
     -- , ((mod4Mask, xK_b), withFocused toggleBorder)
       
     , ((mod4Mask, xK_u), windows S.swapUp)
     , ((mod4Mask, xK_e), windows S.swapDown)] ++

     -- [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
     --       | (key, sc) <- zip [xK_l, xK_z] [0..]
     -- , (f, m) <- [(S.view, 0), (S.shift, shiftMask)]] ++

     -- [ ((m .|. mod4Mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
     --       | (key, sc) <- zip [xK_w, xK_f] [0..]
     -- , (f, m) <- [(S.view, 0), (S.shift, shiftMask)]
     -- ] ++
     [ ((m .|. mod4Mask, k), windows $ f i)
           | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_6] ++ [xK_7 .. xK_9] ++ [xK_0,xK_equal,xK_bar])
           , (f, m) <- [(S.greedyView, 0), (S.shift, shiftMask)]
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
    
