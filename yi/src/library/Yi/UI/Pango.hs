{-# LANGUAGE CPP, ExistentialQuantification, DoRec, TupleSections, NamedFieldPuns, ViewPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- Copyright (c) 2007, 2008 Jean-Philippe Bernardy

-- | This module defines a user interface implemented using gtk2hs and
-- pango for direct text rendering.
module Yi.UI.Pango (start) where

import Prelude (filter)

import Control.Exception (catch, SomeException)
import Control.Concurrent
import Data.Prototype
import Data.IORef
import Data.List (drop, intercalate, zip)
import qualified Data.List.PointedList as PL (moveTo)
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Rope as Rope

import Graphics.UI.Gtk hiding (Region, Window, Action, Point, Style, Modifier, on)
import Graphics.UI.Gtk.Gdk.GC hiding (foreground)
import qualified Graphics.UI.Gtk.Gdk.EventM as EventM
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Gdk.GC as Gtk
import System.Glib.GError

import Yi.Prelude hiding (on)

import Yi.Buffer
import Yi.Config
import Yi.Editor
import Yi.Event
import Yi.Keymap
import Yi.Layout(DividerPosition, DividerRef)
import Yi.Style
import Yi.Tab
import Yi.Window

import qualified Yi.UI.Common as Common
import Yi.UI.Pango.Layouts
import Yi.UI.Pango.Utils
import Yi.UI.TabBar
import Yi.UI.Utils

#ifdef GNOME_ENABLED
import Yi.UI.Pango.Gnome(watchSystemFont)
#endif

-- We use IORefs in all of these datatypes for all fields which could
-- possibly change over time.  This ensures that no 'UI', 'TabInfo',
-- 'WinInfo' will ever go out of date.

data UI = UI
    { uiWindow    :: Gtk.Window
    , uiNotebook  :: SimpleNotebook
    , uiStatusbar :: Statusbar
    , tabCache    :: IORef TabCache
    , uiActionCh  :: Action -> IO ()
    , uiConfig    :: UIConfig
    , uiFont      :: IORef FontDescription
    , uiInput     :: IMContext
    }

type TabCache = PL.PointedList TabInfo

-- We don't need to know the order of the windows (the layout manages
-- that) so we might as well use a map
type WindowCache = M.Map WindowRef WinInfo

data TabInfo = TabInfo
    { coreTabKey      :: TabRef
    , layoutDisplay   :: LayoutDisplay
    , miniwindowPage  :: MiniwindowDisplay
    , tabWidget       :: Widget
    , windowCache     :: IORef WindowCache
    , fullTitle       :: IORef String
    , abbrevTitle     :: IORef String
    }

instance Show TabInfo where
    show t = show (coreTabKey t)

data WinInfo = WinInfo
    { coreWinKey      :: WindowRef
    , coreWin         :: IORef Window
    , shownTos        :: IORef Point
    , lButtonPressed  :: IORef Bool 
    , insertingMode   :: IORef Bool
    , inFocus         :: IORef Bool
    , winLayoutInfo   :: MVar WinLayoutInfo
    , winMetrics      :: FontMetrics
    , textview        :: DrawingArea
    , modeline        :: Label
    , winWidget       :: Widget -- ^ Top-level widget for this window.
    }

data WinLayoutInfo = WinLayoutInfo {
   winLayout :: !PangoLayout,
   tos :: !Point,
   bos :: !Point,
   bufEnd :: !Point,
   cur :: !Point,
   buffer :: !FBuffer,
   regex :: !(Maybe SearchExp)
 }

instance Show WinInfo where
    show w = show (coreWinKey w)

instance Ord EventM.Modifier where
  x <= y = fromEnum x <= fromEnum y

mkUI :: UI -> Common.UI
mkUI ui = Common.dummyUI
    { Common.main          = main
    , Common.end           = const end
    , Common.suspend       = windowIconify (uiWindow ui)
    , Common.refresh       = refresh ui
    , Common.layout        = doLayout ui
    , Common.reloadProject = const reloadProject
    }

updateFont :: UIConfig -> IORef FontDescription -> IORef TabCache -> Statusbar
                  -> FontDescription -> IO ()
updateFont cfg fontRef tc status font = do
    maybe (return ()) (fontDescriptionSetFamily font) (configFontName cfg)
    maybe (return ()) (fontDescriptionSetSize font . fromIntegral) (configFontSize cfg)
    writeIORef fontRef font
    widgetModifyFont status (Just font)
    tcs <- readIORef tc
    forM_ tcs $ \tabinfo -> do
      wcs <- readIORef (windowCache tabinfo)
      forM_ wcs $ \wininfo -> do
        withMVar (winLayoutInfo wininfo) $ \WinLayoutInfo{winLayout} -> layoutSetFontDescription winLayout (Just font)
        -- This will cause the textview to redraw
        widgetModifyFont (textview wininfo) (Just font)
        widgetModifyFont (modeline wininfo) (Just font)

askBuffer :: Window -> FBuffer -> BufferM a -> a
askBuffer w b f = fst $ runBuffer w b f

-- | Initialise the ui
start :: UIBoot
start cfg ch outCh ed = catchGError (startNoMsg cfg ch outCh ed) (\(GError _dom _code msg) -> fail msg)

startNoMsg :: UIBoot
startNoMsg cfg ch outCh ed = do
  logPutStrLn "startNoMsg"
  discard unsafeInitGUIForThreadedRTS

  win   <- windowNew
  ico   <- loadIcon "yi+lambda-fat-32.png"
  vb    <- vBoxNew False 1    -- Top-level vbox
    
  im <- imMulticontextNew
  imContextSetUsePreedit im False  -- handler for preedit string not implemented  
  im `on` imContextCommit $ mapM_ (\k -> ch $ Event (KASCII k) [])  -- Yi.Buffer.Misc.insertN for atomic input?

  set win [ windowDefaultWidth  := 700
          , windowDefaultHeight := 900
          , windowTitle         := "Yi"
          , windowIcon          := Just ico
          , containerChild      := vb
          ]
  win `on` deleteEvent $ io $ mainQuit >> return True
  win `on` keyPressEvent $ handleKeypress ch im
  
  paned <- hPanedNew
  tabs <- simpleNotebookNew
  panedAdd2 paned (baseWidget tabs)

  status  <- statusbarNew
  -- statusbarGetContextId status "global"

  set vb [ containerChild := paned
         , containerChild := status
         , boxChildPacking status := PackNatural
         ]

  fontRef <- newIORef undefined

  let actionCh = outCh . singleton
  tc <- newIORef =<< newCache ed actionCh

#ifdef GNOME_ENABLED
  let watchFont = watchSystemFont
#else
  let watchFont = (fontDescriptionFromString "Monospace 10" >>=)
#endif
  watchFont $ updateFont (configUI cfg) fontRef tc status

  -- use our magic threads thingy (http://haskell.org/gtk2hs/archives/2005/07/24/writing-multi-threaded-guis/)
  discard $ timeoutAddFull (yield >> return True) priorityDefaultIdle 50

  widgetShowAll win

  let ui = UI win tabs status tc actionCh (configUI cfg) fontRef im

  -- Keep the current tab focus up to date
  let move n pl = maybe pl id (PL.moveTo n pl)
      runAction = uiActionCh ui . makeAction
  -- why does this cause a hang without postGUIAsync?
  simpleNotebookOnSwitchPage (uiNotebook ui) $ \n -> postGUIAsync $
    runAction (modA tabsA (move n) :: EditorM ())

  return (mkUI ui)


main :: IO ()
main = logPutStrLn "GTK main loop running" >> mainGUI

-- | Clean up and go home
end :: IO ()
end = mainQuit

-- | Modify GUI and the 'TabCache' to reflect information in 'Editor'.
updateCache :: UI -> Editor -> IO ()
updateCache ui e = do
       cache <- readRef $ tabCache ui
       -- convert to a map for convenient lookups
       let cacheMap = mapFromFoldable . fmap (\t -> (coreTabKey t, t)) $ cache

       -- build the new cache
       cache' <- forM (e ^. tabsA) $ \tab ->
         case M.lookup (tkey tab) cacheMap of
           Just t -> updateTabInfo e ui tab t >> return t
           Nothing -> newTab e ui tab

       -- store the new cache
       writeRef (tabCache ui) cache'

       -- update the GUI
       simpleNotebookSet (uiNotebook ui) =<< forM cache' (\t -> (tabWidget t,) <$> readIORef (abbrevTitle t))


-- | Modify GUI and given 'TabInfo' to reflect information in 'Tab'.
updateTabInfo :: Editor -> UI -> Tab -> TabInfo -> IO ()
updateTabInfo e ui tab tabInfo = do
    -- update the window cache
    wCacheOld <- readIORef (windowCache tabInfo)
    wCacheNew <- mapFromFoldable <$> (forM (tab ^. tabWindowsA) $ \w ->
      case M.lookup (wkey w) wCacheOld of
        Just wInfo -> updateWindow e ui w wInfo >> return (wkey w, wInfo)
        Nothing -> (wkey w,) <$> newWindow e ui w)
    writeIORef (windowCache tabInfo) wCacheNew

    -- TODO update renderer, etc?

    let lookupWin w = wCacheNew M.! w

    -- set layout
    layoutDisplaySet (layoutDisplay tabInfo) . fmap (winWidget . lookupWin) . tabLayout $ tab

    -- set minibox
    miniwindowDisplaySet (miniwindowPage tabInfo) . fmap (winWidget . lookupWin . wkey) . tabMiniWindows $ tab

    -- set focus
    setWindowFocus e ui tabInfo . lookupWin . wkey . tabFocus $ tab

updateWindow :: Editor -> UI -> Window -> WinInfo -> IO ()
updateWindow e _ui win wInfo = do
    writeIORef (inFocus wInfo) False -- see also 'setWindowFocus'
    writeIORef (coreWin wInfo) win
    writeIORef (insertingMode wInfo) (askBuffer win (findBufferWith (bufkey win) e) $ getA insertingA)

setWindowFocus :: Editor -> UI -> TabInfo -> WinInfo -> IO ()
setWindowFocus e ui t w = do
  win <- readIORef (coreWin w)
  let bufferName = shortIdentString (commonNamePrefix e) $ findBufferWith (bufkey win) e
      ml = askBuffer win (findBufferWith (bufkey win) e) $ getModeLine (commonNamePrefix e)
      im = uiInput ui

  writeIORef (inFocus w) True -- see also 'updateWindow'
  update (textview w) widgetIsFocus True
  update (modeline w) labelText ml
  writeIORef (fullTitle t) bufferName
  writeIORef (abbrevTitle t) (tabAbbrevTitle bufferName)
  drawW <- catch (fmap Just $ widgetGetDrawWindow $ textview w)
                 (\(_ :: SomeException) -> return Nothing)
  imContextSetClientWindow im drawW
  imContextFocusIn im

getWinInfo :: UI -> WindowRef -> IO WinInfo
getWinInfo ui ref =
  let tabLoop []     = error "Yi.UI.Pango.getWinInfo: window not found"
      tabLoop (t:ts) = do
        wCache <- readIORef (windowCache t)
        case M.lookup ref wCache of
          Just w -> return w
          Nothing -> tabLoop ts
  in readIORef (tabCache ui) >>= (tabLoop . toList)

-- | Make the cache from the editor and the action channel
newCache :: Editor -> (Action -> IO ()) -> IO TabCache
newCache e actionCh = mapM (mkDummyTab actionCh) (e ^. tabsA)

-- | Make a new tab, and populate it
newTab :: Editor -> UI -> Tab -> IO TabInfo
newTab e ui tab = do
  t <- mkDummyTab (uiActionCh ui) tab
  updateTabInfo e ui tab t
  return t

-- | Make a minimal new tab, without any windows. This is just for bootstrapping the UI; 'newTab' should normally be called instead.
mkDummyTab :: (Action -> IO ()) -> Tab -> IO TabInfo
mkDummyTab actionCh tab = do
    ws <- newIORef M.empty
    ld <- layoutDisplayNew
    layoutDisplayOnDividerMove ld (handleDividerMove actionCh)
    mwp <- miniwindowDisplayNew
    tw <- vBoxNew False 0
    set tw [containerChild := baseWidget ld,
            containerChild := baseWidget mwp,
            boxChildPacking (baseWidget ld) := PackGrow,
            boxChildPacking (baseWidget mwp) := PackNatural]
    ftRef <- newIORef ""
    atRef <- newIORef ""
    return (TabInfo (tkey tab) ld mwp (toWidget tw) ws ftRef atRef)


-- | Make a new window.
newWindow :: Editor -> UI -> Window -> IO WinInfo
newWindow e ui w = do
    let b = findBufferWith (bufkey w) e
    f <- readIORef (uiFont ui)

    ml <- labelNew Nothing
    widgetModifyFont ml (Just f)
    set ml [ miscXalign := 0.01 ] -- so the text is left-justified.
    widgetSetSizeRequest ml 0 (-1) -- allow the modeline to be covered up, horizontally

    v <- drawingAreaNew
    widgetModifyFont v (Just f)
    widgetAddEvents v [Button1MotionMask]
    widgetModifyBg v StateNormal $ mkCol False $ Yi.Style.background $ baseAttributes $ configStyle $ uiConfig ui

    sw <- scrolledWindowNew Nothing Nothing
    scrolledWindowAddWithViewport sw v
    scrolledWindowSetPolicy sw PolicyAutomatic PolicyNever

    box <- if isMini w
     then do
      prompt <- labelNew (Just $ miniIdentString b)
      widgetModifyFont prompt (Just f)

      hb <- hBoxNew False 1
      set hb [ containerChild := prompt,
               containerChild := sw,
               boxChildPacking prompt := PackNatural,
               boxChildPacking sw := PackGrow]

      return (castToBox hb)
     else do
      vb <- vBoxNew False 1
      set vb [ containerChild := sw,
               containerChild := ml,
               boxChildPacking ml := PackNatural]
      return (castToBox vb)

    tosRef    <- newIORef (askBuffer w b (getMarkPointB =<< fromMark <$> askMarks))
    context   <- widgetCreatePangoContext v
    layout    <- layoutEmpty context
    layoutRef <- newMVar (WinLayoutInfo layout 0 0 0 0 (findBufferWith (bufkey w) e) Nothing)
    language  <- contextGetLanguage context
    metrics   <- contextGetMetrics context f language
    ifLButton <- newIORef False
    imode     <- newIORef False
    focused   <- newIORef False
    winRef    <- newIORef w

    layoutSetFontDescription layout (Just f)
    layoutSetText layout "" -- stops layoutGetText crashing (as of gtk2hs 0.10.1)

    let ref = wkey w
        win = WinInfo { coreWinKey = ref
                      , coreWin   = winRef
                      , winLayoutInfo = layoutRef
                      , winMetrics = metrics
                      , textview  = v
                      , modeline  = ml
                      , winWidget = toWidget box
                      , shownTos  = tosRef
                      , lButtonPressed = ifLButton
                      , insertingMode = imode
                      , inFocus = focused
                      }
    updateWindow e ui w win

    v `on` buttonPressEvent   $ handleButtonClick   ui ref
    v `on` buttonReleaseEvent $ handleButtonRelease ui win
    v `on` scrollEvent        $ handleScroll        ui win
    v `on` configureEvent     $ handleConfigure     ui  -- todo: allocate event rather than configure?
    v `on` motionNotifyEvent  $ handleMove          ui win
    discard $ v `onExpose` render ui win
    -- also redraw when the window receives/loses focus
    (uiWindow ui) `on` focusInEvent $ io (widgetQueueDraw v) >> return False 
    (uiWindow ui) `on` focusOutEvent $ io (widgetQueueDraw v) >> return False 
    -- todo: consider adding an 'isDirty' flag to WinLayoutInfo,
    -- so that we don't have to recompute the Attributes when focus changes.
    return win

refresh :: UI -> Editor -> IO ()
refresh ui e = do
    postGUIAsync $ do
       contextId <- statusbarGetContextId (uiStatusbar ui) "global"
       statusbarPop  (uiStatusbar ui) contextId
       discard $ statusbarPush (uiStatusbar ui) contextId $ intercalate "  " $ statusLine e

    updateCache ui e -- The cursor may have changed since doLayout
    cache <- readRef $ tabCache ui
    forM_ cache $ \t -> do
        wCache <- readIORef (windowCache t)
        forM_ wCache $ \w -> do
            updateWinInfoForRendering e ui w
            widgetQueueDraw (textview w)

{- | Record all the information we need for rendering.

This information is kept in an MVar so that the PangoLayout and tos/bos/buffer are in sync.
-}

updateWinInfoForRendering :: Editor -> UI -> WinInfo -> IO ()
updateWinInfoForRendering e _ui w = modifyMVar_ (winLayoutInfo w) $ \wli -> do
  win <- readIORef (coreWin w)
  return $! wli{buffer=findBufferWith (bufkey win) e,regex=currentRegex e}

-- | Tell the 'PangoLayout' what colours to draw, and draw the 'PangoLayout' and the cursor onto the screen
render :: UI -> WinInfo -> t -> IO Bool
render ui w _event = withMVar (winLayoutInfo w) $ \WinLayoutInfo{winLayout=layout,tos,bos,cur,buffer=b,regex} -> do
  -- read the information
  win <- readIORef (coreWin w)

  -- add color attributes.
  let picture = askBuffer win b $ attributesPictureAndSelB sty regex (mkRegion tos bos)
      sty = extractValue $ configTheme (uiConfig ui)
      strokes = [(start',s,end') | ((start', s), end') <- zip picture (drop 1 (fmap fst picture) ++ [bos]),
                  s /= emptyAttributes]
      rel p = fromIntegral (p - tos)
      allAttrs = concat $ do
        (p1, Attributes fg bg _rv bd itlc udrl, p2) <- strokes
        return $ [ AttrForeground (rel p1) (rel p2) (mkCol True fg)
                 , AttrBackground (rel p1) (rel p2) (mkCol False bg)
                 , AttrStyle      (rel p1) (rel p2) (if itlc then StyleItalic     else StyleNormal)
                 , AttrUnderline  (rel p1) (rel p2) (if udrl then UnderlineSingle else UnderlineNone)
                 , AttrWeight     (rel p1) (rel p2) (if bd   then WeightBold      else WeightNormal)
                 ]

  layoutSetAttributes layout allAttrs

  drawWindow <- widgetGetDrawWindow $ textview w
  gc <- gcNew drawWindow

  -- see Note [PangoLayout width]
  -- draw the layout
  drawLayout drawWindow gc 1 0 layout

  -- calculate the cursor position
  im <- readIORef (insertingMode w)

  -- check focus, and decide whether we want a wide cursor
  bufferFocused <- readIORef (inFocus w)
  uiFocused <- Gtk.windowHasToplevelFocus (uiWindow ui)
  let focused = bufferFocused && uiFocused
      wideCursor = 
       case configCursorStyle (uiConfig ui) of
         AlwaysFat -> True
         NeverFat -> False
         FatWhenFocused -> focused
         FatWhenFocusedAndInserting -> focused && im
 

  (PangoRectangle (succ -> curX) curY curW curH, _) <- layoutGetCursorPos layout (rel cur)
  -- tell the input method
  imContextSetCursorLocation (uiInput ui) (Rectangle (round curX) (round curY) (round curW) (round curH))
  -- paint the cursor
  gcSetValues gc (newGCValues { Gtk.foreground = mkCol True $ Yi.Style.foreground $ baseAttributes $ configStyle $ uiConfig ui,
                                Gtk.lineWidth = if wideCursor then 2 else 1 })
  -- tell the renderer
  if im 
  then -- if we are inserting, we just want a line
    drawLine drawWindow gc (round curX, round curY) (round $ curX + curW, round $ curY + curH)
  else do -- if we aren't inserting, we want a rectangle around the current character
    PangoRectangle (succ -> chx) chy chw chh <- layoutIndexToPos layout (rel cur)
    drawRectangle drawWindow gc False (round chx) (round chy) (if chw > 0 then round chw else 8) (round chh)

  return True

doLayout :: UI -> Editor -> IO Editor
doLayout ui e = do
    updateCache ui e
    tabs <- readRef $ tabCache ui
    f <- readRef (uiFont ui)
    heights <- fold <$> mapM (getHeightsInTab ui f e) tabs
    let e' = (tabsA ^: fmap (mapWindows updateWin)) e
        updateWin w = case M.lookup (wkey w) heights of
                          Nothing -> w
                          Just (h,rgn) -> w { height = h, winRegion = rgn }

    -- Don't leak references to old Windows
    let forceWin x w = height w `seq` winRegion w `seq` x
    return $ (foldl . tabFoldl) forceWin e' (e' ^. tabsA)

getHeightsInTab :: UI -> FontDescription -> Editor -> TabInfo -> IO (M.Map WindowRef (Int,Region))
getHeightsInTab ui f e tab = do
  wCache <- readIORef (windowCache tab)
  forM wCache $ \wi -> do
    (_, h) <- widgetGetSize $ textview wi
    win <- readIORef (coreWin wi)
    let metrics = winMetrics wi
        lineHeight = ascent metrics + descent metrics
    let b0 = findBufferWith (bufkey win) e
    rgn <- shownRegion ui f wi b0
    let ret= (round $ fromIntegral h / lineHeight, rgn)
    return ret

shownRegion :: UI -> FontDescription -> WinInfo -> FBuffer -> IO Region
shownRegion ui f w b = modifyMVar (winLayoutInfo w) $ \wli -> do
   (tos, cur, bos, bufEnd) <- updatePango ui f w b (winLayout wli)
   return (wli{tos,cur=clampTo tos bos cur,bos,bufEnd}, mkRegion tos bos)
 where clampTo lo hi x = max lo (min hi x)
-- during scrolling, cur might not lie between tos and bos, so we clamp it to avoid Pango errors

{-
Note [PangoLayout width]
~~~~~~~~~~~~~~~~~~~~~~~~

We start rendering the PangoLayout one pixel from the left of the rendering area, which means a few +/-1 offsets in Pango rendering and point lookup code.
The reason for this is to support the "wide cursor", which is 2 pixels wide. If we started rendering the PangoLayout 
directly from the left of the rendering area instead of at a 1-pixel offset, then the "wide cursor" would only be half-displayed
when the cursor is at the beginning of the line, and would then be a "thin cursor".

An alternative would be to special-case the wide cursor rendering at the beginning of the line, and draw it one pixel to
the right of where it "should" be. I haven't tried this out to see how it looks.

Reiner
-}

-- we update the regex and the buffer to avoid holding on to potential garbage.
-- These will be overwritten with correct values soon, in
-- updateWinInfoForRendering.
updatePango :: UI -> FontDescription -> WinInfo -> FBuffer -> PangoLayout -> IO (Point, Point, Point, Point)
updatePango ui font w b layout = do
  (width_', height') <- widgetGetSize $ textview w
  let width' = max 0 (width_' - 1) -- see Note [PangoLayout width]

  oldFont <- layoutGetFontDescription layout
  oldFontStr <- maybe (return Nothing) (fmap Just . fontDescriptionToString) oldFont
  newFontStr <- Just <$> fontDescriptionToString font
  when (oldFontStr /= newFontStr) (layoutSetFontDescription layout (Just font))

  win <- readIORef (coreWin w)
  let [width'', height''] = fmap fromIntegral [width', height']
      metrics             = winMetrics w
      lineHeight          = ascent metrics + descent metrics
      winh                = max 1 $ floor (height'' / lineHeight)

      (tos, size, point, text)  = askBuffer win b $ do
                              from     <- getMarkPointB =<< fromMark <$> askMarks
                              rope     <- streamB Forward from
                              p        <- pointB
                              bufEnd     <- sizeB
                              let content = fst $ Rope.splitAtLine winh rope
                              -- allow BOS offset to be just after the last line
                              let addNL = if Rope.countNewLines content == winh
                                              then id
                                              else (++"\n")
                              return (from, bufEnd, p, addNL $ Rope.toString content)

  if configLineWrap $ uiConfig ui
    then do oldWidth <- layoutGetWidth layout
            when (oldWidth /= Just width'') (layoutSetWidth layout $ Just width'')
    else do (Rectangle px _py pwidth _pheight, _) <- layoutGetPixelExtents layout
            widgetSetSizeRequest (textview w) (px+pwidth) (-1)

  -- optimize for cursor movement
  oldText <- layoutGetText layout
  when (oldText /= text) (layoutSetText layout text)

  (_, bosOffset, _) <- layoutXYToIndex layout width'' (fromIntegral winh * lineHeight - 1)
  return (tos, point, tos + fromIntegral bosOffset + 1, size)

reloadProject :: IO ()
reloadProject = return ()

mkCol :: Bool -- ^ is foreground?
      -> Yi.Style.Color -> Gtk.Color
mkCol True  Default = Color 0 0 0
mkCol False Default = Color maxBound maxBound maxBound
mkCol _ (RGB x y z) = Color (fromIntegral x * 256)
                            (fromIntegral y * 256)
                            (fromIntegral z * 256)

-- * GTK Event handlers

-- | Process GTK keypress if IM fails
handleKeypress :: (Event -> IO ()) -- ^ Event dispatcher (Yi.Core.dispatch)
                  -> IMContext
                  -> EventM EKey Bool
handleKeypress ch im = do
  gtkMods <- eventModifier
  gtkKey  <- eventKeyVal
  ifIM    <- imContextFilterKeypress im
  let char = keyToChar gtkKey
      modsWithShift = M.keys $ M.filter (`elem` gtkMods) modTable
      mods | isJust char = filter (/= MShift) modsWithShift
           | otherwise   = modsWithShift
      key  = case char of
        Just c  -> Just $ KASCII c
        Nothing -> M.lookup (keyName gtkKey) keyTable
  
  case (ifIM, key) of
    (True, _   ) -> return ()
    (_, Nothing) -> logPutStrLn $ "Event not translatable: " ++ show key
    (_, Just k ) -> io $ ch $ Event k mods
  return True

-- | Map GTK long names to Keys
keyTable :: M.Map String Key
keyTable = M.fromList
    [("Down",       KDown)
    ,("Up",         KUp)
    ,("Left",       KLeft)
    ,("Right",      KRight)
    ,("Home",       KHome)
    ,("End",        KEnd)
    ,("BackSpace",  KBS)
    ,("Delete",     KDel)
    ,("Page_Up",    KPageUp)
    ,("Page_Down",  KPageDown)
    ,("Insert",     KIns)
    ,("Escape",     KEsc)
    ,("Return",     KEnter)
    ,("Tab",        KTab)
    ,("ISO_Left_Tab", KTab)
    ]

-- | Map Yi modifiers to GTK 
modTable :: M.Map Modifier EventM.Modifier
modTable = M.fromList
    [ (MShift, EventM.Shift  )
    , (MCtrl,  EventM.Control)
    , (MMeta,  EventM.Alt    )
    , (MSuper, EventM.Super  )
    , (MHyper, EventM.Hyper  )
    ]

-- | Same as Gtk.on, but discards the ConnectId
on :: object -> Signal object callback -> callback -> IO ()
on widget signal handler = discard $ Gtk.on widget signal handler

handleButtonClick :: UI -> WindowRef -> EventM EButton Bool
handleButtonClick ui ref = do
  (x, y) <- eventCoordinates
  click  <- eventClick
  button <- eventButton
  io $ do
    w <- getWinInfo ui ref
    point <- pointToOffset (x, y) w
    
    let focusWindow = focusWindowE ref
        runAction = uiActionCh ui . makeAction
    
    runAction focusWindow
    case (click, button) of
      (SingleClick, LeftButton) ->  do
        io $ writeIORef (lButtonPressed w) True
        win <- io $ readIORef (coreWin w)
        runAction $ do
          b <- gets $ bkey . findBufferWith (bufkey win)
          withGivenBufferAndWindow0 win b $ do
            m <- selMark <$> askMarks
            setMarkPointB m point
            moveTo point
            setVisibleSelection False
      _ -> return ()
    
    return True

handleButtonRelease :: UI -> WinInfo -> EventM EButton Bool
handleButtonRelease ui w = do
  (x, y)   <- eventCoordinates
  button   <- eventButton
  io $ do
    point <- pointToOffset (x, y) w
    disp  <- widgetGetDisplay $ textview w
    cb    <- clipboardGetForDisplay disp selectionPrimary
    case button of
         MiddleButton -> pasteSelectionClipboard ui w point cb
         LeftButton   -> setSelectionClipboard   ui w cb >>
                         writeIORef (lButtonPressed w) False
         _            -> return ()
  return True

handleScroll :: UI -> WinInfo -> EventM EScroll Bool
handleScroll ui w = do
  scrollDirection <- eventScrollDirection
  xy <- eventCoordinates
  io $ do
    ifPressed <- readIORef $ lButtonPressed w
    -- query new coordinates
    let editorAction = do
          withBuffer0 $ scrollB $ case scrollDirection of
            ScrollUp   -> negate configAmount
            ScrollDown -> configAmount
            _          -> 0 -- Left/right scrolling not supported
        configAmount = configScrollWheelAmount $ uiConfig ui
    uiActionCh ui (makeAction editorAction)
    if ifPressed
     then selectArea ui w xy
     else return ()
  return True

handleConfigure :: UI -> EventM EConfigure Bool
handleConfigure ui = do
  -- trigger a layout
  -- why does this cause a hang without postGUIAsync?
  io $ postGUIAsync $ uiActionCh ui (makeAction (return () :: EditorM()))
  return False -- allow event to be propagated
  
handleMove :: UI -> WinInfo -> EventM EMotion Bool
handleMove ui w = eventCoordinates >>= (io . selectArea ui w) >>
                  return True

handleDividerMove :: (Action -> IO ()) -> DividerRef -> DividerPosition -> IO ()
handleDividerMove actionCh ref pos = actionCh (makeAction (setDividerPosE ref pos))

-- | Convert point coordinates to offset in Yi window
pointToOffset :: (Double, Double) -> WinInfo -> IO Point
pointToOffset (x,y) w = withMVar (winLayoutInfo w) $ \WinLayoutInfo{winLayout,tos,bufEnd} -> do
  im <- readIORef (insertingMode w)
  (_, charOffsetX, extra) <- layoutXYToIndex winLayout (max 0 (x-1)) y -- see Note [PangoLayout width]
  return $ min bufEnd (tos + fromIntegral (charOffsetX + if im then extra else 0))

selectArea :: UI -> WinInfo -> (Double, Double) -> IO ()
selectArea ui w (x,y) = do
  p <- pointToOffset (x,y) w
  let editorAction = do
        txt <- withBuffer0 $ do
          moveTo p
          setVisibleSelection True
          readRegionB =<< getSelectRegionB
        setRegE txt
  
  uiActionCh ui (makeAction editorAction)
  -- drawWindowGetPointer (textview w) -- be ready for next message.

pasteSelectionClipboard :: UI -> WinInfo -> Point -> Clipboard -> IO ()
pasteSelectionClipboard ui w p cb = do
  win <- io $ readIORef (coreWin w)
  let cbHandler Nothing    = return ()
      cbHandler (Just txt) = uiActionCh ui $ makeAction $ do
        b <- gets $ bkey . findBufferWith (bufkey win)
        withGivenBufferAndWindow0 win b $ do
          pointB >>= setSelectionMarkPointB
          moveTo p
          insertN txt
  clipboardRequestText cb cbHandler

-- | Set selection clipboard contents to current selection
setSelectionClipboard :: UI -> WinInfo -> Clipboard -> IO ()
setSelectionClipboard ui _w cb = do
  -- Why uiActionCh doesn't allow returning values?
  selection <- newIORef ""
  let yiAction = do
        txt <- withEditor $ withBuffer0 $ readRegionB =<< getSelectRegionB :: YiM String
        io $ writeIORef selection txt
  uiActionCh ui $ makeAction yiAction
  txt <- readIORef selection
  
  if (not . null) txt
    then clipboardSetText cb txt
    else return () 



-- Some useful stuff from `startNoMsg`
--
-- Disable the left pane (file/module browser) until Shim/Scion discussion has
-- concluded. Shim causes crashes, but it's not worth fixing if we'll soon
-- replace it.

{-
tabs' <- notebookNew
widgetSetSizeRequest tabs' 200 (-1)
notebookSetTabPos tabs' PosBottom
panedAdd1 paned tabs'

-- Create the tree views for files and modules
(filesProject, modulesProject) <- loadProject =<< getCurrentDirectory

filesStore   <- treeStoreNew [filesProject]
modulesStore <- treeStoreNew [modulesProject]

filesTree   <- projectTreeNew (outCh . singleton) filesStore
modulesTree <- projectTreeNew (outCh . singleton) modulesStore

scrlProject <- scrolledWindowNew Nothing Nothing
scrolledWindowAddWithViewport scrlProject filesTree
scrolledWindowSetPolicy scrlProject PolicyAutomatic PolicyAutomatic
notebookAppendPage tabs scrlProject "Project"

scrlModules <- scrolledWindowNew Nothing Nothing
scrolledWindowAddWithViewport scrlModules modulesTree
scrolledWindowSetPolicy scrlModules PolicyAutomatic PolicyAutomatic
notebookAppendPage tabs scrlModules "Modules"
-}
