
{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (catch)

import XMonad hiding (splitVertically, splitHorizontallyBy)
import XMonad.Config.Desktop

import qualified XMonad.StackSet as S

import qualified Data.Map as M
import Data.Monoid
import Data.Maybe (fromMaybe,isNothing,catMaybes)
import Data.List (find,null,partition)
import Data.IORef
import Data.Ratio
import Data.Typeable

import Control.Monad
import Control.Monad.Trans hiding (liftIO)
import Control.Arrow (second)
import Control.Exception

import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers -- isFullscreen, doFullFloat
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.RestoreMinimized
import XMonad.Hooks.DynamicHooks

import XMonad.Layout.Dishes
import XMonad.Layout.Mosaic
import XMonad.Layout.MosaicAlt
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
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
--import XMonad.Layout.Maximize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.ResizableTile
import XMonad.Layout.Named
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.StackTile
--import XMonad.Layout.IndependentScreens
--import XMonad.Layout.ThreeColumns
import XMonad.Layout.MultiColumns
import XMonad.Layout.WindowNavigation
import XMonad.Layout.LayoutModifier

import XMonad.Actions.NoBorders
import XMonad.Actions.CycleWindows
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
import XMonad.Actions.DeManage
import XMonad.Actions.UpdateFocus hiding (adjustEventInput)

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.WindowProperties
import XMonad.Util.NamedScratchpad
import XMonad.Util.WorkspaceCompare
import XMonad.Util.Replace

import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.XPropManage

import System.Exit
import System.Process
import System.Posix

import Text.Regex.PCRE

import Network.BSD

import DBus.Types
import DBus.Client.Simple

import qualified Data.Text as T
import Control.Concurrent.MVar

import Foreign.C.String
import Foreign.C.Types

-- project switching abhängig vom workspace switching
-- switch zu neuem workspace soll projekt nicht komplett unloaden, der state
-- wird assoziert mit dem vorigen workspace so das er wieder hergestellt
-- werden kann ohne die startup hooks zu benutzen
dbusEmitChangedWorkspace :: Client -> IO ()
dbusEmitChangedWorkspace client = do
  emit client (objectPath_ $ T.pack "/")
              (interfaceName_ $ T.pack "org.xmonad")
              (memberName_ $ T.pack "ChangedWorkspace")
              [ toVariant $ "foo" ]

  -- client <- connectSession
  -- requestName client (busName_ $ T.pack "org.xmonad") []
  -- export client (objectPath_ $ T.pack "/hello")
  --        [ method (interfaceName_ $ T.pack "org.xmonad") (memberName_ $ T.pack "hello") sayHello ]
  -- mvar <- newEmptyMVar
  -- takeMVar mvar

-- TODO extract colors from .Xdefaults
foregroundWhite = "#ffffff"
backgroundBlack = "#000000"

-- darkYellow = "#654119"
darkYellow = "#634324"
darkRed = "#611E1E"

black = "#3A3A3A"
red = "#F92673"
green = "#9AD22A"
yellow = "#FD951D"
blue = "#66DDEF"
magenta = "#9B6DFE"
cyan = "#5E7175"
white = "#DFDFDA"

intenseBlack = "#303030"
intenseRed = "#F96098"
intenseGreen = "#A7CF58"
intenseYellow = "#ff8c00"
--intenseYellow = "#B0763F"
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

myNormalBorderColor  = black
myFocusedBorderColor = intenseYellow
myBorderWidth = 1

myPP (Just h) = defaultPP
         { ppCurrent = wrap ("^fg(" ++ red ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)[") "]^p(2)^fg()^bg()"
         , ppVisible = wrap ("^fg(" ++ blue ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)[") "]^p(2)^fg()^bg()"
         , ppHidden = wrap ("^fg(" ++ green ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)") "^p(2)^fg()^bg()"
         , ppHiddenNoWindows = wrap ("^fg(" ++ white ++ ")^bg(" ++ backgroundBlack ++ ")^p(2)") "^p(2)^fg()^bg()"
         , ppSep     = " ^fg(grey60)^r(1x12)^fg() "
         , ppLayout  = dzenColor white "" . (\x ->
                                                 let tokens = words x
                                                     xs = filter (=="ReflectX") tokens
                                                     ys = filter (=="ReflectY") tokens
                                                     name = head $ filter (\t -> (all (/=t) [ "Mirror"
                                                                                            , "Mirrored"
                                                                                            , "Minimize"
                                                                                            , "Maximize"
                                                                                            , "ReflectX"
                                                                                            , "ReflectY"
                                                                                            , "TwoScreen"
                                                                                            , "Spacing"
                                                                                            , "1","2","3","4","5","6","7","8","9"])) tokens
                                                 in case name of
                                                      "Full" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-full.xbm)"
                                                      "Spiral" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-spiral.xbm)"
                                                      "FixedColumn" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-tall-right.xbm)"
                                                      "ResizableTall" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-mosaic.xbm)"
                                                      "ResizableThreeCol" -> pad "^i(/home/lazor/icons/dzen/dzen/layout-threecol.xbm)"
                                                      otherwise -> pad name
                                            )
         , ppUrgent  = dzenColor backgroundBlack yellow . wrap "!" "!"
         , ppTitle   = \_ -> ""
         , ppSort    = getSortByIndex >>= \f -> return $ f . namedScratchpadFilterOutWorkspace
         --, ppExtras  = [logMail]
         , ppOutput  = hPutStrLn h -- . (++) " "
         }
myPP Nothing = defaultPP

--
-- XMonad.Layout.Maximize
--

data Maximize a = Maximize (Maybe Window) deriving ( Read, Show )

maximize :: LayoutClass l Window => l Window -> ModifiedLayout Maximize l Window
maximize = ModifiedLayout $ Maximize Nothing

data MaximizeRestore = MaximizeRestore Window deriving ( Typeable, Eq )
instance Message MaximizeRestore

maximizeRestore :: Window -> MaximizeRestore
maximizeRestore = MaximizeRestore

instance LayoutModifier Maximize Window where
    modifierDescription (Maximize _) = "Maximize"
    pureModifier (Maximize (Just target)) rect (Just (S.Stack focused _ _)) wrs =
            if focused == target
                then (maxed ++ rest, Nothing)
                else (rest ++ maxed, Nothing)
        where
            (toMax, rest) = partition (\(w, _) -> w == target) wrs
            maxed = map (\(w, _) -> (w, maxRect)) toMax
            maxRect = Rectangle (rect_x rect) (rect_y rect)
                (rect_width rect) (rect_height rect)
    pureModifier _ _ _ wrs = (wrs, Nothing)

    pureMess (Maximize mw) m = case fromMessage m of
        Just (MaximizeRestore w) -> case mw of
            Just w' -> if (w == w')
                        then Just $ Maximize Nothing   -- restore window
                        else Just $ Maximize $ Just w  -- maximize different window
            Nothing -> Just $ Maximize $ Just w        -- maximize window
        _ -> Nothing


--
-- ControlFocus
--

data FocusState = FocusState
                { focus_history :: M.Map WorkspaceId [Window] }

adjustEventInput :: X ()
adjustEventInput = withDisplay $ \dpy -> do
  rootw <- asks theRoot
  io $ selectInput dpy rootw $  substructureRedirectMask .|. substructureNotifyMask
                                .|. enterWindowMask .|. leaveWindowMask .|. structureNotifyMask
                                .|. buttonPressMask .|. pointerMotionMask .|. focusChangeMask
                                .|. propertyChangeMask

testFocusEvent e@(AnyEvent { ev_event_type = et }) | et == focusIn =
                                                       do io $ putStrLn "FocusIn"
                                                          return $ All True
                                                   | et == focusOut =
                                                       do io $ putStrLn "FocusOut"
                                                          return $ All True
                                                   | et == createNotify =
                                                       do io $ putStrLn "CreateNotify"
                                                          return $ All True
testFocusEvent e@(CrossingEvent { ev_event_type = et }) | et == enterNotify =
                                                            do io $ putStrLn "EnterNotify"
                                                               return $ All True
                                                        | et == leaveNotify =
                                                            do io $ putStrLn "LeaveNotify"
                                                               return $ All True
testFocusEvent e@(DestroyWindowEvent {}) = do
  when ((ev_event_type e) == destroyNotify) $ io $ putStrLn "DestroyNotify"
  return $ All True
testFocusEvent e@(PropertyEvent { ev_atom = a, ev_window = w }) = do
  d <- asks display
  root <- asks theRoot
  c <- io $ internAtom d "_NET_CURRENT_DESKTOP" True
  when (w == root && a == c) $ do
    v :: (Maybe [CInt]) <- io $ rawGetWindowProperty 32 d a root
    io $ putStrLn $ show v
  return $ All True
testFocusEvent _ = return $ All True

--focusChangeHandler (PropertyEvent {ev_event_display = d, ev_window = w, ev_atom = a}) = do



currentNumWindows :: X Int
currentNumWindows = do
    ws <- gets windowset
    return $ case ( S.stack $ S.workspace $ S.current ws ) of
               (Just s) -> (length $ S.up s) + (length $ S.down s) + (M.size $ S.floating ws) + 1
               otherwise -> 0

currentNumDesktops :: X ()
currentNumDesktops = do
  d <- asks display
  root <- asks theRoot
  a <- io $ internAtom d "_NET_NUMBER_OF_DESKTOPS" True
  t <- io $ getTextProperty d root a
  l <- io $ wcTextPropertyToTextList d t
  io $ putStrLn $ show l


type LastNumWindowsRef = IORef (M.Map WorkspaceId Int)
type LastFocusRef = IORef (M.Map WorkspaceId [Window])
type LastMousePosRef = IORef (Int,Int)

controlFocus :: LastNumWindowsRef -> LastFocusRef -> LastMousePosRef -> X ()
controlFocus lastnumwin lastfocus lastmouse = do
  ws <- gets windowset
  dpy <- asks display
  root <- asks theRoot
  (_,_,_,current_x,current_y,_,_,_) <- io $ queryPointer dpy root
  let w = S.workspace $ S.current ws
      wi = S.tag w
      ms = S.stack w
  case ms of
    (Just s) -> do
      let current_num_windows = (length $ S.up s) + (length $ S.down s) + (M.size $ S.floating ws) + 1
      prev_num_windows <- io $ readIORef lastnumwin >>= return . fromMaybe 0 . M.lookup wi
      if (current_num_windows /= prev_num_windows)
       -- this alternative deals with newly opened or closed windows
       then do
        io $ writeIORef lastmouse (fromIntegral current_x, fromIntegral current_y)
        io $ putStrLn $ "Writing " ++ (show current_num_windows) ++ " to NumWindowsRef"
        io $ modifyIORef lastnumwin (\m -> M.insert wi current_num_windows m)
        if (current_num_windows < prev_num_windows)
         -- user just closed a window -> restore focus
         then restoreFocusN lastfocus lastmouse 1 >> return ()
         -- user opened a window -> remember focus (put currently focused window at position 0)
         else rememberFocusN lastfocus lastmouse 0
       -- this alternative deals with focus changes through mouse movement _only_
       -- (focus changes through keyboard events are handled seperatly by focusUpRemember/focusDownRemember)
       else do
         m <- liftIO $ readIORef lastfocus
         (last_x,last_y) <- liftIO $ readIORef lastmouse
         case M.lookup wi m of
           (Just wins) -> do
                  if (last_x,last_y) == (fromIntegral current_x,fromIntegral current_y)
                   then return ()
                   else do
                     io $ writeIORef lastmouse (fromIntegral current_x, fromIntegral current_y)
                     liftIO $ putStrLn $ "Remembered Mouse Position at " ++ (show (fromIntegral current_x, fromIntegral current_y))
                     rememberFocusN lastfocus lastmouse 0
           Nothing -> return ()
    otherwise -> return ()

rememberFocusN :: LastFocusRef -> LastMousePosRef -> Int -> X ()
rememberFocusN r m n = do
  ws <- gets windowset
  dpy <- asks display
  root <- asks theRoot
  (_,_,_,current_x,current_y,_,_,_) <- io $ queryPointer dpy root
  io $ writeIORef m (fromIntegral current_x, fromIntegral current_y)
  let w = S.workspace $ S.current ws
      wi = S.tag w
      ms = S.stack w
  case ms of
    (Just s) -> do
            mxs <- io $ atomicModifyIORef r (\m ->
                                    case M.lookup wi m of
                                      (Just wins) ->
                                          let wins' = case n of
                                                        0 -> [S.focus s] ++ (take 10 wins)
                                                        1 -> [(cycle wins) !! 0, S.focus s] ++ drop 1 (take 10 wins)
                                                        2 -> [(cycle wins) !! 0, (cycle wins) !! 1, S.focus s] ++ drop 2 (take 10 wins)
                                                        otherwise -> (take 10 wins)
                                          in if (cycle wins) !! n == S.focus s
                                              then (m, Nothing)
                                              else (M.insert wi wins' m, Just wins')
                                      otherwise -> (M.insert wi [S.focus s] m, Just [S.focus s]))
            case mxs of
             (Just xs) -> do
               liftIO $ putStrLn $ "Remembered that " ++ (show $ fromIntegral $ S.focus s) ++ " had focus at position " ++ (show n)
               liftIO $ putStrLn $ show xs
             otherwise -> return ()
    otherwise -> return ()

rememberFocus :: LastFocusRef -> LastMousePosRef -> X ()
rememberFocus r m = rememberFocusN r m 0

focusUpRemember :: LastFocusRef -> LastMousePosRef -> X ()
focusUpRemember r m = do
  windows S.focusUp
  rememberFocusN r m 0

focusDownRemember :: LastFocusRef -> LastMousePosRef -> X ()
focusDownRemember r m = do
  windows S.focusDown
  rememberFocusN r m 0

focusMasterRemember :: LastFocusRef -> LastMousePosRef -> X ()
focusMasterRemember r m = do
  windows S.focusMaster
  rememberFocusN r m 0

restoreFocusN :: LastFocusRef -> LastMousePosRef -> Int -> X (Maybe Window)
restoreFocusN r m n = do
  ws <- gets windowset
  let w = S.workspace $ S.current ws
      wi = S.tag w
      ms = S.stack w
  case ms of
    (Just s) -> do
            mwin <- io $ atomicModifyIORef r (\m ->
                                      case M.lookup wi m of
                                        (Just wins) ->
                                            let wins' = case n of
                                                          0 -> (drop 1 (take 10 wins)) ++ [(cycle wins) !! 0]
                                                          1 -> (drop 1 (take 10 wins)) ++ (filter (==((cycle wins) !! 0)) (take 10 wins))
                                                          2 -> [(cycle (take 10 wins)) !! 1, (cycle wins) !! 0] ++ drop 2 (take 10 wins)
                                                          otherwise -> take 10 wins
                                                w = head wins'
                                            in (M.insert wi wins' m, Just (w,wins'))
                                        otherwise -> (m,Nothing))
            case mwin of
              (Just (w,wins')) -> do
               liftIO $ putStrLn $ "Restoring focus to " ++ (show $ fromIntegral w)
               liftIO $ putStrLn $ show wins'
               -- restore focus only if window is still on workspace
               if any (==w) $ (S.up s) ++ [S.focus s] ++ (S.down s)
                then do
                  windows $ S.focusWindow w
                  return $ Just w
                else return Nothing
              Nothing -> return Nothing
    otherwise -> return Nothing

restoreFocus :: LastFocusRef -> LastMousePosRef -> X (Maybe Window)
restoreFocus r m = restoreFocusN r m 0

--
-- ManageHook
--

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

-- breaks build when uncommentedf
-- toManageHook :: (X a) -> Query a
-- toManageHook = Query . lift

type SessionFloatsRef = IORef [String]

sessionFloatsMaybeManageHook :: SessionFloatsRef -> MaybeManageHook
sessionFloatsMaybeManageHook ref = do
  names <- io $ readIORef ref
  pc <- stringProperty "WM_CLASS"
  pn <- stringProperty "WM_NAME"
  if any (==pn) names || any (==pc) names
     then fmap Just doFloat
     else return $ Nothing

sessionFloat :: SessionFloatsRef -> LastFocusRef -> LastMousePosRef -> Window -> X ()
sessionFloat ref lastFocusRef lastMousePosRef w = do
  ps <- withDisplay (\d -> fmap catMaybes $ mapM (getStringProperty d w) ["WM_CLASS","WM_NAME"])
  case ps of
    (p:_) -> do
            (io $ readIORef ref) >>= \strings -> case () of
                                                   _ | null strings || all (/=p) strings -> do
                                                           windows $ S.float w $ S.RationalRect (1%4) (1%4) (1%2) (1%2)
                                                           io $ modifyIORef ref (\sf -> p : sf)
                                                     | otherwise -> do
                                                           rememberFocusN lastFocusRef lastMousePosRef 1
                                                           windows $ S.sink w
                                                           io $ modifyIORef ref (\sf -> filter (/=p) sf)
    otherwise -> return ()


-- createSessionFloat :: SessionFloatsRef -> Window -> X ()
-- createSessionFloat ref w = do
--   ps <- withDisplay (\d -> fmap catMaybes $ mapM (getStringProperty d w) ["WM_CLASS","WM_NAME"])
--   case ps of
--     (p:_) -> io $ modifyIORef ref (\sf -> p : sf)
--     otherwise -> return ()

-- removeSessionFloat :: SessionFloatsRef -> Window -> X ()
-- removeSessionFloat ref w = do
--   ps <- withDisplay (\d -> fmap catMaybes $ mapM (getStringProperty d w) ["WM_CLASS","WM_NAME"])
--   case ps of
--     (p:_) -> io $ modifyIORef ref (\sf -> filter (/=p) sf)
--     otherwise -> return ()

myManageHook sessionfloats lastfocus = composeAll
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

      , (fmap (=~ "^gkrellm") $ stringProperty "WM_NAME") -?> doIgnore
      , (fmap (=~ "^VLC .GLX output.") $ stringProperty "WM_NAME") -?> doCenterFloat

      , (fmap (=~ "^JACK.Audio.Connection.Kit.*") $ stringProperty "WM_NAME") -?> doFloat

      , sessionFloatsMaybeManageHook sessionfloats

      , stringProperty "WM_NAME" =? "*Remember*" -?> doCenterFloat

      , stringProperty "WM_NAME" =? "*Help*" -?> do
                                      insertPosition Below Newer
      , stringProperty "WM_NAME" =? "*compilation*" -?> insertPosition Below Older
      , stringProperty "WM_NAME" =? "*haskell*" -?> insertPosition Below Older
      , stringProperty "WM_NAME" =? "*ack*" -?> insertPosition Below Older

      , stringProperty "WM_WINDOW_ROLE" =? "Manager" -?> insertPosition Below Older

      , return True -?> do
          insertPosition Below Newer
      ]
    ]

--
-- TwoScreen Layout
--

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

--
--
--

data ResizableThreeCol a = ResizableThreeCol !Int !Rational !Rational [Rational] deriving (Show,Read)

instance LayoutClass ResizableThreeCol a where
    doLayout (ResizableThreeCol nmaster _ frac mfrac) r =
        return . (\x->(x,Nothing)) .
        ap zip (tile3 frac (mfrac ++ repeat 1) r nmaster . length) . S.integrate
    handleMessage (ResizableThreeCol nmaster delta frac mfrac) m =
        return $ msum [fmap resize     (fromMessage m)
                      ,fmap incmastern (fromMessage m)]
            where resize Shrink = ResizableThreeCol nmaster delta (max 0 $ frac-delta) mfrac
                  resize Expand = ResizableThreeCol nmaster delta (min 1 $ frac+delta) mfrac
                  mresize MirrorShrink s = mresize' s delta
                  mresize MirrorExpand s = mresize' s (0-delta)
                  mresize' s d = let n = length $ S.up s
                                     total = n + (length $ S.down s) + 1
                                     pos = if n == (nmaster-1) || n == (total-1) then n-1 else n
                                     mfrac' = modifymfrac (mfrac ++ repeat 1) d pos
                                 in ResizableTall nmaster delta frac $ take total mfrac'
                  modifymfrac [] _ _ = []
                  modifymfrac (f:fx) d n | n == 0    = f+d : fx
                                         | otherwise = f : modifymfrac fx d (n-1)
                  incmastern (IncMasterN d) = ResizableThreeCol (max 0 (nmaster+d)) delta frac mfrac
    description _ = "ResizableThreeCol"

-- | tile3.  Compute window positions using 3 panes
tile3 :: Rational -> [Rational] -> Rectangle -> Int -> Int -> [Rectangle]
tile3 f mf r nmaster n
    | n <= nmaster || nmaster == 0 = splitVertically mf n r
    | n <= nmaster+1 = splitVertically mf nmaster s1 ++ splitVertically (drop nmaster mf) (n-nmaster) s2
    | otherwise = splitVertically mf nmaster r1 ++ splitVertically mf nmid r2 ++ splitVertically mf nright r3
  where (r1, r2, r3) = split3HorizontallyBy f r
        (s1, s2) = splitHorizontallyBy f r
        nslave = (n - nmaster)
        nmid = (n - nmaster - nright)
        nright = ceiling (nslave % 2)

split3HorizontallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle, Rectangle)
split3HorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy midw sh
    , Rectangle (sx + fromIntegral leftw + fromIntegral midw) sy rightw sh )
  where leftw = ceiling $ fromIntegral sw * (2/3) * f
        midw = ceiling ( (sw - leftw) % 2 )
        rightw = sw - leftw - midw

splitVertically :: RealFrac r => [r] -> Int -> Rectangle -> [Rectangle]
splitVertically [] _ r = [r]
splitVertically _ n r | n < 2 = [r]
splitVertically (f:fx) n (Rectangle sx sy sw sh) = Rectangle sx sy sw smallh :
    splitVertically fx (n-1) (Rectangle sx (sy+fromIntegral smallh) sw (sh-smallh))
  where smallh = min sh (floor $ fromIntegral (sh `div` fromIntegral n) * f) --hmm, this is a fold or map.

splitHorizontallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw-fromIntegral leftw) sh)
  where leftw = floor $ fromIntegral sw * f

--
-- Layouts
--

-- data MIRROR = MIRROR deriving (Read, Show, Eq, Typeable)
-- instance Transformer MIRROR Window where
--     transform _ x k = k (Mirror x)

myLayout =
    configurableNavigation noNavigateBorders $
    minimize $
    maximize $
    --boringWindows $
    mkToggle (single REFLECTX) $
    mkToggle (single REFLECTY) $
    mkToggle (single MIRROR) $
    -- spacing 4 $
    reflectHoriz $
    -- smartBorders $
    boringAuto $
    (mkTwoScreen 0 resizable full) ||| (mkTwoScreen 0 threecol full)
  where
    mosaicalt = MosaicAlt M.empty
    dishes = Dishes 2 (2/6)
    tiled  = Tall 1 (3/100) (1/2)
    full = Full
    fixed = FixedColumn 1 5 120 10
    stack = StackTile 2 (3/100) (1/2)
    downspiral = reflectVert $ SP.spiralWithDir SP.South SP.CW (2/6)
    leftspiral = SP.spiralWithDir SP.East SP.CW (6/7)
    resizable = ResizableTall 1 (3/100) (50/100) []
    mirrorresizable = Mirror $ ResizableTall 2 (2/100) (70/100) []
    threecol = ResizableThreeCol 1 (2/100) (50/100) []
    multicol = multiCol [1] 2 0.01 0.55
    tall = Tall 1 (3/100) (55/100)
    mirrortall = Mirror $ Tall 2 (3/100) (70/100)


myUpdatePointer = updatePointer (Relative 0.99 0.1)

switchScreenToDesktop sc d = do
    mi <- screenWorkspace sc
    whenJust mi $ (\i ->
                    windows (\s ->
                             let c = S.currentTag s
                             in S.view c $ S.greedyView d $ S.view i s))


data Toggles = Struts | Maximized Window
            deriving (Eq,Ord,Show)
type RememberToggles = IORef (M.Map Toggles Bool)

main = do
  lastnumwin <- newIORef M.empty
  lastfocus <- newIORef M.empty
  lastmouse <- newIORef (0,0)
  toggles <- newIORef M.empty

  sessionfloats <- newIORef []

  -- BEWARE OF THE UGLY HACK!
  spawn "/bin/bash -e /home/lazor/bin/resolution.sh > /tmp/resolution"
  spawn "/bin/bash -e /home/lazor/bin/resolution-offset.sh > /tmp/resolution-offset"
  sleep 1
  resolution <- readFile "/tmp/resolution"
  resolution_offset <- readFile "/tmp/resolution-offset"
  putStr resolution
  putStr resolution_offset

  r <- (try (return $! read resolution_offset :: IO Int) :: IO (Either SomeException Int))
  offset <- case r of
              Left e -> return 0
              Right x -> return x

  hn <- getHostName
  h <- case hn of
         "spirit" -> do
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
                           h3 <- spawnPipe $ "dzen-launcher.pl -w 275 -h 23 -y 0 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                                        -- ++ "-w 212 -x 260 -y 786 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                                        -- ++ "-l \\\"^fg\\(" ++ yellow ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/cpu.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o --- "
                                        -- ++ "-l \\\"^fg\\(" ++ magenta ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/mem.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ magenta ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5 --- "
                                        -- ++ "-w 200 -x 1166 -y 786 -fg \\\"" ++ yellow ++ "\\\" -bg black -p -ta r -fn \\\"" ++ dzenFontMono hn ++ "\\\" --- "
                                        -- ++ "-w 150 -x 472 -y 786 -fg \\\"" ++ green ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                                        -- ++ "-l \\\"^fg\\(" ++ green ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/bat_full_02.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ green ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5 --- "
                                        -- ++ "-w 555 -x 610 -y 786 -fg \\\"" ++ red ++ "\\\" -bg black -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                                        -- ++ "-l \\\"^fg\\(" ++ red ++ "\\)^i\\(/home/lazor/icons/dzen/xbm8x8/wifi_01.xbm\\)^fg\\(\\) \\\" -fg \\\"" ++ red ++ "\\\" -bg \\\"" ++ gray ++ "\\\" -s o -ss 1 -sw 5"
                           -- spawn "sleep 5; trayer --align left --edge bottom --SetDockType true --SetPartialStrut true --expand true --width 300 --height 18 --transparent true --tint 0x000000 --widthtype pixel --margin 880"
                           return h3
                 return $ Just h2
         "capcom" -> do
                 h2 <- spawnPipe $
                       "dzen-launcher.pl -x " ++ (show offset) ++ " -w 325 -h 25 -y 0 -fg \\\"" ++ yellow ++ "\\\" -bg \\\"" ++ backgroundBlack ++ "\\\" -p -ta l -fn \\\"" ++ dzenFont hn ++ "\\\" --- "
                 return $ Just h2
         otherwise -> spawnPipe "dzen2" >>= return . Just

  replace
  xmonad $ applyMyKeyBindings sessionfloats lastfocus lastmouse toggles $ desktopConfig
        { terminal = "konsole"
        , modMask = mod4Mask -- use the Windows button as mod
        , logHook =
            --currentWorkspaceOnTop >>
            ewmhDesktopsLogHook >>
            setWMName "LG3D" >>
            (dynamicLogWithPP $ myPP h) >>
            controlFocus lastnumwin lastfocus lastmouse
        , manageHook =
            manageHook desktopConfig <+>
            myManageHook sessionfloats lastfocus
        , layoutHook = desktopLayoutModifiers $ myLayout
        , workspaces = ["1","2","3","4","5","6","7","8","9","10","11","12"]
        , normalBorderColor = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor
        , borderWidth = myBorderWidth
        , focusFollowsMouse = True
        , handleEventHook = ewmhDesktopsEventHook
                            `mappend` fullscreenEventHook
                            `mappend` restoreMinimizedEventHook
                            -- `mappend` testFocusEvent
                            --`mappend` positionStoreEventHook
        , startupHook = do
            startupHook desktopConfig
            switchScreenToDesktop 1 "11"
            adjustEventInput
        }


applyMyKeyBindings sessionfloats lastFocusRef lastMousePosRef toggles conf =
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
    , (mod4Mask, xK_h)
    ]
    `additionalKeys`
    ([ ((mod4Mask, xK_F1),
        spawn "kdesudo -u lazor /home/lazor/bin/xemacsclient.sh")
     , ((mod4Mask, xK_F2),
        spawn "urxvt -title '*Remember*' -e bash -c \"/home/lazor/bin/remember-launcher.sh '-e (make-remember-frame-terminal)'\"")
     , ((mod4Mask, xK_F3),
        spawn "xdg-open ~/.local/share/applications/conkeror.desktop")
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
     , ((mod4Mask, xK_Escape),
        -- >> spawn "/bin/rm /home/lazor/.xmonad/dosystrayfix"
        spawn "xmonad --restart")
     , ((mod4Mask .|. shiftMask, xK_Escape),
        -- spawn "touch /home/lazor/.xmonad/dosystrayfix"
        spawn "dbus-send --print-reply --dest=org.kde.ksmserver /KSMServer org.kde.KSMServerInterface.logout int32:1 int32:-1 int32:1")
     , ((mod4Mask, xK_space), sendMessage NextLayout) --(windows S.shiftMaster) >> sendMessage NextLayout)
     , ((mod4Mask, xK_BackSpace), refresh)




-- LEFT HAND

     -- \
     , ((mod4Mask, xK_backslash), focusDownRemember lastFocusRef lastMousePosRef)
     , ((mod4Mask .|. shiftMask, xK_backslash), focusUpRemember lastFocusRef lastMousePosRef)
     -- Tab
     , ((mod4Mask, xK_Tab), restoreFocusN lastFocusRef lastMousePosRef 2 >> return ())

     -- y
     , ((mod4Mask, xK_y),  toggleWS)
     --, ((mod4Mask .|. shiftMask, xK_y), sendMessage $ Toggle REFLECTY)
     , ((mod4Mask .|. shiftMask, xK_y), do sendMessage $ Toggle MIRROR)
     -- w
     , ((mod4Mask, xK_w), prevWS)
     , ((mod4Mask .|. shiftMask, xK_w), shiftToPrev)

     -- f
     , ((mod4Mask, xK_f), nextWS)
     , ((mod4Mask .|. shiftMask, xK_f), shiftToNext)

     -- g
     , ((mod4Mask, xK_g), withFocused $ \w -> sessionFloat sessionfloats lastFocusRef lastMousePosRef w)

     -- p
     , ((mod4Mask, xK_p), nextScreen >> shiftPrevScreen >> prevScreen   )

     -- a
     , ((mod4Mask, xK_a), nextScreen)
     , ((mod4Mask .|. shiftMask, xK_a), shiftNextScreen >> nextScreen)
     -- r
     , ((mod4Mask, xK_r), sendMessage Expand)
     , ((mod4Mask .|. shiftMask, xK_r), sendMessage MirrorExpand)
     -- s
     , ((mod4Mask, xK_s), sendMessage Shrink)
     , ((mod4Mask .|. shiftMask, xK_s), sendMessage MirrorShrink)
     -- t
     , ((mod4Mask, xK_t), withFocused $ \w -> windows (S.shiftMaster . S.sink w))
     , ((mod4Mask .|. shiftMask, xK_t), do
          withFocused $ \focused ->
              windows (\s ->
                           case (S.index s) of
                             [] -> s
                             (master:_) -> if master == focused
                                            then S.focusMaster $ S.swapUp s
                                            else S.shiftMaster $ S.sink focused s
                      )
          rememberFocus lastFocusRef lastMousePosRef
       )

     --, ((mod4Mask .|. shiftMask, xK_t), sendMessage $ Toggle REFLECTX)
     -- d
     , ((mod4Mask, xK_d), shiftNextScreen)

     -- +
     , ((mod4Mask, xK_plus), sendMessage RestoreNextMinimizedWin)

     -- q
     , ((mod4Mask, xK_q), withFocused minimizeWindow)
     , ((mod4Mask .|. shiftMask, xK_q), kill)

     -- x
     , ((mod4Mask, xK_x), windows S.swapDown)
     , ((mod4Mask .|. shiftMask, xK_x), sendMessage $ IncMasterN (-1))

     -- c
     , ((mod4Mask, xK_c), windows S.swapUp)
     , ((mod4Mask .|. shiftMask, xK_c), sendMessage $ IncMasterN 1)

     -- v
     , ((mod4Mask, xK_v), do
          withFocused $ \w -> do
            ts <- io $ readIORef toggles
            case (M.lookup (Maximized w) ts, M.lookup Struts ts) of
              x | x == (Just False, Just False) ||
                  x == (Just False, Nothing) ||
                  x == (Nothing, Just False) ||
                  x == (Nothing, Nothing) -> do
                                sendMessage $ maximizeRestore w
                                sendMessage ToggleStruts
                                io $ modifyIORef toggles (M.insert (Maximized w) True)
                                io $ modifyIORef toggles (M.insert Struts True)
                | x == (Just True, Just True) -> do
                                sendMessage $ maximizeRestore w
                                sendMessage ToggleStruts
                                io $ modifyIORef toggles (M.insert (Maximized w) False)
                                io $ modifyIORef toggles (M.insert Struts False)
                | x == (Just True, Just False) ||
                  x == (Just True, Nothing) -> do
                                sendMessage $ maximizeRestore w
                                io $ modifyIORef toggles (M.insert (Maximized w) False)
                | x == (Just False, Just True) ||
                  x == (Nothing, Just True) -> do
                                sendMessage ToggleStruts
                                io $ modifyIORef toggles (M.insert Struts False)
       )

     -- b
     , ((mod4Mask, xK_b), withFocused toggleBorder)


-- RIGHT HAND

     -- j
     -- l
     , ((mod4Mask, xK_l), prevWS)
     , ((mod4Mask .|. shiftMask, xK_l), shiftToPrev >> prevWS)

     -- u
     , ((mod4Mask, xK_u), do sendMessage $ Go U; rememberFocusN lastFocusRef lastMousePosRef 0)
     , ((mod4Mask .|. shiftMask, xK_u), sendMessage $ Swap U)

     -- z
     , ((mod4Mask, xK_z), nextWS )
     , ((mod4Mask .|. shiftMask, xK_z), shiftToNext >> nextWS)
     -- - [ ]
     -- h
     -- n
     , ((mod4Mask, xK_n), do sendMessage $ Go L; rememberFocusN lastFocusRef lastMousePosRef 0)
     , ((mod4Mask .|. shiftMask, xK_n), sendMessage $ Swap L)
     -- e
     , ((mod4Mask, xK_e), do sendMessage $ Go D; rememberFocusN lastFocusRef lastMousePosRef 0)
     , ((mod4Mask .|. shiftMask, xK_e), sendMessage $ Swap D)

     -- i
     , ((mod4Mask, xK_i), do sendMessage $ Go R; rememberFocusN lastFocusRef lastMousePosRef 0)
     , ((mod4Mask .|. shiftMask, xK_i), sendMessage $ Swap R)

     -- o
     -- ~

     -- k
     -- m

     -- - . '


     ] ++

     (concat [[ ((mod4Mask, key), do
                   rememberFocusN lastFocusRef lastMousePosRef 1;
                   windows $ S.greedyView workspace;
                   restoreFocusN lastFocusRef lastMousePosRef 1;
                   return ())
              , ((shiftMask .|. mod4Mask, key), windows $ S.shift workspace)
              , ((controlMask, key), do
                   rememberFocusN lastFocusRef lastMousePosRef 1;
                   windows $ S.greedyView workspace;
                   restoreFocusN lastFocusRef lastMousePosRef 1;
                   return ())
              , ((shiftMask .|. controlMask, key), windows $ S.shift workspace)
              ] | (key,workspace) <- [ (xK_1,"1")
                                     , (xK_2,"2")
                                     , (xK_3,"3")
                                     , (xK_4,"4")
                                     , (xK_5,"5")
                                     , (xK_6,"6")
                                     , (xK_7,"7")
                                     , (xK_8,"8")
                                     , (xK_9,"9")
                                     , (xK_0,"0")
                                     , (xK_ampersand,"11")
                                     , (xK_bar,"12") ]]))
