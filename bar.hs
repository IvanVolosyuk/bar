-- Copyright 2012 Google Inc. All Rights Reserved.
-- Author: vol@google.com (Ivan Volosyuk)

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State
import Data.Bits
import Data.Time
import Data.Maybe
import Foreign.C
import GHC.Ptr (nullPtr)
import Graphics.X11.Xft
import Graphics.X11.Xrender
import Numeric
import System.Exit
import Text.Printf (printf)
import System.Locale

import DzenParse
import Icon

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib.Font
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))

dzenMsg = "^fg(white)^bg(#2b4f98) 1 ^fg()^bg()^fg(black)^bg(#cccccc) 2 ^fg()^bg()^fg(black)^bg(#cccccc) 3 ^fg()^bg()^fg(black)^bg(#cccccc) 9 ^fg()^bg()  ^fg(#202020){56623107}dzen.sh (~/.xmonad/ivan) - GVIM^fg()"

padding = 4 :: Int
barX = 0 :: Int
barY = 0
barHeight = 24 :: Int
barWidth = 1366 :: Int
backgroundColor = 0xBEBEBE
backgroundColorString = "#BEBEBE"
foregroundColorString = "#000000"
graphBackgroundColor = 0x181838
cpuColorTable = [0x70FF70, 0xFF8080, 0xF020F0, 0x3030FF] -- FIXME: one color is unused
memColorTable = [0x007F00, 0xFF0000, 0x0000FF]
netColorTable = [0x0000FF, graphBackgroundColor, 0x00FF00]
netSilenceThreshold = 100
marginTop = 1 :: Int
marginBottom = 1 :: Int
marginsVertical = marginTop + marginBottom :: Int
fontName = "-*-fixed-medium-r-normal--15-*-*-*-*-*-iso10646-*"
fontXft = True
clockTimeFormat = "%R"

-- widgets = [
--   0.1, 5, cpu { colors = [0x123, 0x234, 0x456], padding=0, noframe=True}
 
wc = defaultWidgetConfig
loadWidgets = [ -- widget   width refresh
--   (staticMessageWidget, 100, 1), -- FIXME need builder, not widget
  {- (makeCpuWidget, ww, 0.025),
   (makeCpuWidget, ww, 0.05),
   (makeCpuWidget, ww, 0.10),
   (makeCpuWidget, ww, 0.25),
   (makeCpuWidget, ww, 0.50), -}
   (makeClockWidget, 60, 1, wc),
   (makeCpuWidget, 80, 1, wc),
   (makeMemWidget, 20, 10, wc),
   (makeBatteryhWidget, 100, 3, wc),
   (makeNetWidget "wlan0", 40, 0.5, wc),
   (makeTitleWidget, 1028, 0, wc { drawFrame = False })
   ]

loadTooltipWidgets = [
   (makeClockWidget, 60, 1, wc),
   (makeCpuWidget, 80, 0.03, wc),
   (makeNetWidget "wlan0", 80, 0.05, wc)
   ]

height = barHeight - marginsVertical :: Int

newtype IOBox a = IOBox { exec :: BoxIO a }
type BoxIO a = IO (a, IOBox a)

type ControlChan = Chan (Window,CInt)

type ExtendedState = (GlobalState, Widget)

data WidgetConfig = WidgetConfig {
  widgetWidth :: Int,
  widgetId :: CInt,
  refreshRate :: Int,
  widgetX :: Int,
  drawFrame :: Bool,
  widgetTooltop :: WidgetLine
  }
defaultWidgetConfig = WidgetConfig {
    widgetWidth = 100,
    widgetId = 0,
    refreshRate = 100000,
    widgetX = 0,
    drawFrame = True,
    widgetTooltop = (makeCpuWidget, 60, 0.1, wc)
    }

data RenderState = RenderState { getDisplay :: Display, getDrawable::Pixmap, getGC :: GC}
data WindowRenderState = WindowRenderState Window GC
data GlobalState = GlobalState { getIconCache :: IconCache,
                                 getFont :: Either XftFont FontSet,
                                 widgetsById :: M.Map Window (M.Map CInt Widget),
                                 mainWindow :: Window,
                                 tooltipWindow :: Maybe Window,
                                 globalChan :: ControlChan
                               }

data Widget = Widget {
    onMeasure :: StateT ExtendedState IO Int,
    onDraw :: Int -> StateT ExtendedState IO (),
    doUpdate :: StateT ExtendedState IO (),
    onDestroy :: StateT ExtendedState IO (),
    widgetConfig :: WidgetConfig,
    getRenderState :: RenderState,
    getWindowRenderState :: WindowRenderState
}
type MakeWidget = ControlChan -> Widget -> IO Widget
type WidgetLine = (MakeWidget, Int, Double, WidgetConfig)

data WidgetFunctions st = WidgetFunctions {
    onMeasureFunc :: st -> StateT ExtendedState IO Int,
    onDrawFunc :: st -> Int -> StateT ExtendedState IO (),
    doUpdateFunc :: st -> StateT ExtendedState IO (),
    onDestroyFunc :: st -> StateT ExtendedState IO ()
}

defaultFunctions = WidgetFunctions {
  onMeasureFunc = simpleOnMeasure,
  onDrawFunc = \_ _ -> return (),
  doUpdateFunc = \_ -> return (),
  onDestroyFunc = \_ -> return ()
  }

defaultWidget rs wrs wc = Widget {
  onMeasure = return 1,
  onDraw = \_ -> return (),
  doUpdate = return (),
  onDestroy = return (),
  widgetConfig = wc,
  getRenderState = rs,
  getWindowRenderState = wrs
  }

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

drawWindow rs (WindowRenderState w gc) = do
  setfg rs backgroundColor
  fillRect rs 0 0 barWidth barHeight

setfg (RenderState dpy w gc) color = setForeground dpy gc color
setbg (RenderState dpy w gc) color = setBackground dpy gc color
fillRect (RenderState dpy win gc) x y w h = fillRectangle dpy win gc (fi x) (fi y) (fi w) (fi h)


drawAllWidgets gState [] pos = return (gState, [])
drawAllWidgets gState widgets pos = do
  print "Draw all widgets!"
  let (widgetId,widget) : otherWidgets = widgets
  (width, (gState, widget)) <- runStateT (onMeasure widget) (gState,widget)
  let widgetPos = pos - width - padding
  let widget2 = widget { widgetConfig = (widgetConfig widget) { widgetX = widgetPos } }

  let rs = (getRenderState widget)
  setfg rs graphBackgroundColor
  fillRect rs widgetPos marginTop width (barHeight - marginsVertical)
  setfg rs 0xFFFFFF

  (gState, widget2) <- execStateT ((onDraw widget2) widgetPos) (gState,widget2) 
  (gState, otherWidgets) <- drawAllWidgets gState otherWidgets (widgetPos)
  return (gState, (widgetId,widget2) : otherWidgets)

data EventResult = Done | ExitEventLoop | Update GlobalState

updateWindow = updateWindowRegion 0 barWidth

updateWindowRegion x width widget = do
  let RenderState dpy buf buf_gc = (getRenderState widget)
  let WindowRenderState w gc = (getWindowRenderState widget)
  copyArea dpy buf w gc x 0 (fi width) (fi barHeight)
                            (fi x) 0
  sync dpy False

handleMessage :: GlobalState -> Event -> IO EventResult
handleMessage _ (ClientMessageEvent {ev_data = (-1):_}) = return ExitEventLoop
handleMessage gState (ClientMessageEvent {ev_window = w, ev_data = widgetId:_}) = do
  -- print $ "Update for widget: " ++ (show widgetId)
  let widgetsMap = widgetsById gState
  case M.lookup w widgetsMap of
   Nothing -> return Done
   Just windowWidgetMap -> do
     case M.lookup widgetId windowWidgetMap of
       Nothing -> return Done
       Just widget -> do
         (gState, widget) <- execStateT (doUpdate widget) (gState,widget)
   
         let x = widgetX . widgetConfig $  widget
         let wConf = widgetConfig widget
         let width = widgetWidth wConf
         when (drawFrame wConf) $ do
           let rs = (getRenderState widget)
           setfg rs graphBackgroundColor -- FIXME: move to widget draw?
           fillRect rs x marginTop width (barHeight - marginsVertical)
   
         (gState, widget) <- execStateT ((onDraw widget) x) (gState, widget) 
         updateWindowRegion (fi x) (fi width) widget
         let windowWidgetMap' = M.insert widgetId widget windowWidgetMap
         let widgetsMap' = M.insert w windowWidgetMap' widgetsMap
         return . Update $ gState { widgetsById = widgetsMap' }
  
handleMessage gState (ButtonEvent {ev_x = x}) = do
  return ExitEventLoop

handleMessage gState (ExposeEvent {ev_window = w}) = do
  let widgetsMap = widgetsById gState
  case M.lookup w widgetsMap of
    Nothing -> return Done
    Just windowWidgetMap -> do
      let widgets = M.toList windowWidgetMap
      let (_,widget) = head widgets
      drawWindow (getRenderState widget) (getWindowRenderState widget)
      (gState, widgets) <- drawAllWidgets gState widgets (fi barWidth)
      print "MapNotify: expensive redraw all"
      updateWindow widget
      return $ Update gState { widgetsById = M.insert w (M.fromList widgets) widgetsMap }

handleMessage gState (MotionEvent {}) = return Done

handleMessage gState (CrossingEvent {ev_event_type = 7, ev_window = ww, ev_event_display = dpy}) = do -- EnterNotify
  liftIO $ print "Enter! Creating Window"
  let scr = (defaultScreen dpy)
  let visual = defaultVisual dpy scr
      attrmask = cWOverrideRedirect
  w <- allocaSetWindowAttributes $ \attributes -> do
         set_override_redirect attributes True
         createWindow dpy (defaultRootWindow dpy) (fi 0) (fi 25)
                        (fi barWidth) (fi barHeight)
                    0 copyFromParent inputOutput visual attrmask attributes

  tooltopAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_TOOLTIP" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi tooltopAtom]

  gc <- createGC dpy w
  setBackground dpy gc (whitePixel dpy scr) -- FIXME: figure out if this is needed

  buf <- createPixmap dpy w (fi barWidth) (fi barHeight) (defaultDepth dpy scr)
  buf_gc <- createGC dpy buf
  setBackground dpy gc backgroundColor
  setLineAttributes dpy gc 1 lineSolid capRound joinRound -- FIXME: use sane attributes for performance

  setBackground dpy gc (whitePixel dpy scr) -- FIXME: figure out if this is needed
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  selectInput dpy w (structureNotifyMask .|. exposureMask)
  mapWindow dpy w

  let rState = RenderState dpy buf buf_gc
  let wrState = WindowRenderState w gc
  widgets <- mapM (initWidget rState wrState (globalChan gState)) $ zip [1..] loadTooltipWidgets
  let widgetsMap = (widgetsById gState)
  let widgetsMap' = M.insert w (M.fromList widgets) widgetsMap
  return . Update $ gState { widgetsById = widgetsMap', tooltipWindow = Just w }
 
handleMessage gState (CrossingEvent {ev_event_type = 8, ev_window = ww, ev_event_display = dpy}) = do -- LeaveNotify
  liftIO $ print "Leave! Destroying Window"
  case tooltipWindow gState of
    Nothing -> return Done
    Just w -> do
      let widgetsMap = widgetsById gState
      let windowWidgetMap = fromMaybe M.empty (M.lookup w widgetsMap)
      mapM (destroyWidget gState) . M.elems $ windowWidgetMap
      destroyWindow dpy w
      return . Update $ gState { widgetsById = M.delete w widgetsMap, tooltipWindow = Nothing }

handleMessage gState (AnyEvent {ev_event_type = 14}) = return Done -- NoExpose

handleMessage _ event = (print $ "Unhandled event:" ++ (eventName event) ++ ": " ++ (show event)) >> return Done

eventMap = M.fromList eventTable

eventLoop dpy gState = do
  allocaXEvent $ \ev -> do
    nextEvent dpy ev
    event <- getEvent ev
    -- print $ show $ event
    res <- handleMessage gState event
    case res of
      ExitEventLoop -> return ()
      Done -> eventLoop dpy gState
      Update gState' -> eventLoop dpy gState'

sendClientEvent d a w val = do
    allocaXEvent $ \e -> do
         setEventType e clientMessage
         setClientMessageEvent e w a 32 val currentTime
         sendEvent d w False structureNotifyMask e
    sync d False

copyChanToX chan = do
  d   <- openDisplay ""
  a <- internAtom d "BAR_UPDATE" False
  forever $ do
     (w,x) <- readChan chan
     -- print $ "Copy to X: " ++ (show x)
     sendClientEvent d a w (fi x) `catch`  \x -> do
       print $ "Exception caught: " ++ (show x)
       return ()

inputReader chan = loopForever `catch` \x -> writeChan chan (-1) where
  loopForever = forever $ do
    line <- getLine
    print $ "Line: " ++ line
    writeChan chan 333

errorHandler dpy err = do
  print "X11 Error!"

main = do
  dpy <- openDisplay "" -- FIXME: proper way to get display name
  let scr = (defaultScreen dpy)
  let visual = defaultVisual dpy scr
  w <- createWindow dpy (defaultRootWindow dpy) (fi barX) (fi barY)
                        (fi barWidth) (fi barHeight)
                    0 copyFromParent inputOutput visual 0 nullPtr
  let strutValues = [0, 0, fi barHeight :: CLong, 0,
                     0, 0, 0, 0,
                     0, 1360, 0, 0]
  strutPartial <- internAtom dpy "_NET_WM_STRUT_PARTIAL" False
  changeProperty32 dpy w strutPartial cARDINAL propModeReplace strutValues
  strut <- internAtom dpy "_NET_WM_STRUT" False
  changeProperty32 dpy w strut cARDINAL propModeReplace (take 4 strutValues)

  dockAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi dockAtom]



  gc <- createGC dpy w
  setBackground dpy gc backgroundColor -- FIXME: figure out if this is needed

  buf <- createPixmap dpy w (fi barWidth) (fi barHeight) (defaultDepth dpy scr)
  buf_gc <- createGC dpy buf
  setBackground dpy gc backgroundColor
  setLineAttributes dpy gc 1 lineSolid capRound joinRound -- FIXME: use sane attributes for performance

  setBackground dpy gc (whitePixel dpy scr) -- FIXME: figure out if this is needed
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  selectInput dpy w (structureNotifyMask .|. buttonPressMask 
                 .|. enterWindowMask .|. leaveWindowMask .|. pointerMotionMask
                 .|. exposureMask)
  mapWindow dpy w

  chan <- newChan
  setErrorHandler errorHandler
  eventCopyThread <- forkOS $ copyChanToX chan

  let rState = RenderState dpy buf buf_gc
  let wrState = WindowRenderState w gc
  widgets <- mapM (initWidget rState wrState chan) $ zip [1..] loadWidgets

  font <- case fontXft of
    False -> createFontSet dpy fontName >>= \(_,_,fontSet) -> return $ Right fontSet
    True -> xftFontOpenXlfd dpy (defaultScreenOfDisplay dpy) fontName >>= return . Left
  iconCache <- makeIconCache dpy
  let gState = GlobalState iconCache font (M.insert w (M.fromList widgets) M.empty) w Nothing chan
  eventLoop dpy gState
  
  killThread eventCopyThread
  mapM (destroyWidget gState) . concat . map M.elems . M.elems . widgetsById $ gState
  destroyWindow dpy w
  closeDisplay dpy

destroyWidget gState w = runStateT (onDestroy w) (gState, w)

initWidget :: RenderState -> WindowRenderState -> ControlChan -> 
              (CInt, (MakeWidget, Int, Double, WidgetConfig)) -> IO (CInt, Widget)
initWidget rs wrs chan (widgetId, (makeWidget, width, refresh, widgetConfig)) = do
  let widgetSetup = defaultWidget rs wrs widgetConfig {
                widgetWidth = width,
                refreshRate = (truncate $ refresh * 1000000),
                widgetId = widgetId }
  widget <- makeWidget chan widgetSetup
  return (widgetId, widget)


data CpuState = CpuState { samples :: [[Int]] }


simpleOnMeasure :: a -> StateT ExtendedState IO Int
simpleOnMeasure _ = do
  (_,widgetState) <- get
  return . widgetWidth . widgetConfig $ widgetState

wrapWidget functions widget state =
  let WidgetFunctions onM onD doU onDe = functions in
  widget { onMeasure = onM state,
           onDraw = onD state,
           doUpdate = doU state,
           onDestroy = onDe state }

makeSegment (x,y) = Segment x' y0' x' y1' where
  x' = fi x
  y0' = (fi $ height - 1 + marginTop)
  y1' = (fi $ height - 1 + marginTop) - (fi y)

drawColorSegments rs [] = return ()
drawColorSegments rs@(RenderState dpy w gc) (x:xs) = do
  let (segments, color) = x
  setfg rs color
  drawSegments dpy w gc segments
  drawColorSegments rs xs

strip :: String -> String
strip s = reverse . dropWhile p . reverse . dropWhile p $ s where
  p = (==' ')

split1 ch s = (x, safeTail xs) where
  safeTail [] = []
  safeTail (x:xs) = xs
  (x,xs) = break (==ch) s

getCpuData = readFile "/proc/stat" >>= return . map(read) . words . head . lines
delta newar ar = map (\(n,o) -> n-o) $ zip newar ar
updateGraph samples sample = newSamples where
  newSamples = map (\(n,o) -> o++[n]) $ zip sample $ map (drop 1) samples
makeCpuSample :: [Int] -> [Dimension]
makeCpuSample (_ :user:nice:sys:idle:io:tail) = map (makeLine total) values where
  (total:values) = reverse $ accumulate 0 [sys + io, nice, user, idle]
accumulate = scanl (+)
safe total = if total /= 0 then total else 1
makeLine total = fi . (`div` safe total) . (* (height - 1))

readKeyValueFile pp filename = readFile filename >>= return . makeMap where
  makeMap l = M.fromList $ map parseLine . lines $ l
  parseLine l = (strip k, pp . words $ v) where
     (k,v) = split1 ':' l

readBatteryFile = readKeyValueFile head
readNetFile = readKeyValueFile $ map read


runBackgroundTask :: ControlChan -> Window -> WidgetConfig -> BoxIO a -> IO (Chan a, ThreadId)
runBackgroundTask masterChan w config boxio = do
  clientChan <- newChan -- FIXME: chan get rid of clientChan parameter?
  threadId <- forkOS $ thread clientChan boxio
  return (clientChan, threadId) where
    refresh = refreshRate config
    n = widgetId config
    thread clientChan boxio = do
      (output, box2) <- boxio
      -- print $ "bgTask sends: " ++ (show n)
      writeChan masterChan (w, n)
      writeChan clientChan output
      threadDelay refresh
      thread clientChan (exec box2)

makeBox :: IO a -> (a -> IO (b,a)) -> BoxIO b
makeBox makeState progressState = do
  inState <- makeState
  box inState where
    box state = do
      (out, state2) <- progressState state
      return (out, IOBox { exec = box state2 })

statelessBox :: IO b -> BoxIO b
statelessBox newState = makeBox (return ()) $ \_ -> do
  st <- newState
  return (st, ())

makeWidgetWithThread makeInitialState makeThread prepareData drawData masterChan widgetSetup = do
  let config = widgetConfig widgetSetup
      initialState = makeInitialState config
      (WindowRenderState win _) = getWindowRenderState widgetSetup
  (clientChan, threadId) <- runBackgroundTask masterChan win config makeThread
  let state = (initialState, clientChan, threadId)
  return $ wrapWidget functions widgetSetup state where
    functions = defaultFunctions { onDrawFunc = draw, doUpdateFunc = update, onDestroyFunc = destroy }
    draw (widgetState,_,_) pos = drawData widgetState pos
    update state = do
      (gState, widget) <- get
      let (widgetState, chan, threadId) = state
      newSample <- liftIO $ readChan chan
      let newWidgetState = prepareData widgetState newSample
      put (gState, wrapWidget functions widget (newWidgetState, chan, threadId))
    destroy (_,_,threadId) = liftIO $ killThread threadId

dropZeros [] = []
dropZeros ((i,0):xs) = dropZeros xs
dropZeros ((i,v):xs) = (i,v) : dropZeros xs

makeGraphWidget colorTable makeThread =
  makeWidgetWithThread initialState makeThread prepareData drawData where
    initialState config = replicate (length colorTable) $ replicate (widgetWidth config) 0
    prepareData state newSample = updateGraph state newSample
    drawData graph pos = do
      (gState, widget) <- get
      let segments = map (\a -> map makeSegment $ dropZeros $ zip [pos..] a) graph -- FIXME: segments using x position
      -- liftIO . print . show . map head . map reverse $ segments
      let rs = getRenderState widget
      liftIO $ drawColorSegments rs $ zip segments colorTable

makeCpuWidget :: MakeWidget
makeCpuWidget = makeGraphWidget cpuColorTable makeThread where
  makeThread = makeBox getCpuData $ \procData -> do
    newProcData <- getCpuData
    let procDelta = delta newProcData procData
    -- print . show $ makeCpuSample procDelta
    return (makeCpuSample procDelta, newProcData)

makeMemSample input = map (makeLine total) values where
  total:free:cached:active:[] = map (read . (input !)) ["MemTotal", "MemFree", "Cached", "Active"]
  values = [total - free, total - free - cached, active]

makeMemWidget :: MakeWidget
makeMemWidget = makeGraphWidget memColorTable makeThread where
  makeThread = statelessBox $ do
    memState <- liftIO $ readBatteryFile "/proc/meminfo"
    return $ makeMemSample memState

makeNetWidget :: String -> MakeWidget
makeNetWidget dev masterChan widgetSetup = makeGraphWidget netColorTable
                                      makeThread masterChan widgetSetup where
  makeThread = makeBox (liftIO $ readNetFile "/proc/net/dev") $ \netState -> do
    newNetState <- liftIO $ readNetFile "/proc/net/dev"
    let netDelta = delta (newNetState ! dev) (netState ! dev)
    -- print $ show $ makeNetSample netDelta
    return ((makeNetSample netDelta), newNetState)
  f x = log (x + 1)
  f2 x = (f x) * (f x)
  maxF = (*2) . f2 . (*10) . fi $ (refreshRate config) -- max rate 10 Mb/s
  config = widgetConfig widgetSetup
  makeNetSample input = map (makeLine total) values where
    atIndex idx = f2 . fi $ input !! idx
    inbound = atIndex 0
    outbound = atIndex 8
    total = truncate maxF
    values = map truncate [maxF, maxF - inbound, outbound] :: [Int]

drawText msg pos = do
  (gState, widget) <- get
  let rs@(RenderState dpy w gc) = (getRenderState widget)
  liftIO $ do
    setbg rs graphBackgroundColor
    setfg rs 0xC7AE86
    case getFont gState of
      Right font ->
       wcDrawString dpy w font gc (fi $ pos + (padding `div` 2)) 17 msg
      Left font ->
        withXftDraw dpy w vis colormap action where
          scr = defaultScreen dpy
          vis = defaultVisual dpy scr
          colormap = defaultColormap dpy scr
          action d = withXftColorName dpy vis colormap "#C7AE86" $ \c -> do
             xftDrawString d c font (pos + (padding `div` 2)) 18 msg

makeBatteryhWidget =
  makeWidgetWithThread initialState makeThread prepareData drawData where
    initialState config = ".."
    prepareData state newMsg = newMsg
    drawData = drawText
    makeThread = statelessBox $ do
      batteryInfo <- readBatteryFile "/proc/acpi/battery/BAT0/info"
      batteryState <- liftIO $ readBatteryFile "/proc/acpi/battery/BAT0/state"
      let capacity = read $ batteryInfo ! "design capacity"
          rate = read $ batteryState ! "present rate" :: Int
          remainingCapacity = read $ batteryState ! "remaining capacity"
          (h, m) = (remainingCapacity * 60 `div` rate) `divMod` 60
          percent = remainingCapacity * 100 `div` capacity
      return $ case batteryState ! "charging state" of
        "discharging" | rate /= 0 -> printf "%d%%(%d:%02d)" percent h m
        otherwise -> printf "%d%%C" percent

makeClockWidget =
  makeWidgetWithThread initialState makeThread prepareData drawData where
    initialState config = ".."
    prepareData state newMsg = newMsg
    drawData = drawText
    makeThread = statelessBox $ do
      time <- liftIO getCurrentTime
      timezone <- liftIO getCurrentTimeZone
      let localtime = utcToLocalTime timezone time
      return $ formatTime defaultTimeLocale clockTimeFormat localtime

makeTitleWidget masterChan = 
  makeWidgetWithThread initialState makeThread prepareData drawData masterChan where
    initialState config = []
    prepareData state newMsg = newMsg
    drawData = drawDzen
    exit _ = writeChan masterChan (0, -1) >> return ""
    makeThread = statelessBox $ do
      line <- getLine `catch` exit
      let annotated = parseLine line
      return annotated

withDefault defaultColor color = case color of
  "" -> defaultColor
  otherwise -> color

drawDzen :: [Message] -> Int -> StateT ExtendedState IO ()
drawDzen input pos = do
  (gState, widget) <- get
  let wConf = widgetConfig widget
  case getFont gState of
     Right font -> liftIO $ print "Non-XFT drawing not implemented" >> return ()
     Left font  -> drawDzenXft font input pos (gState, widget) wConf
  
drawDzenXft :: XftFont -> [Message] -> Int -> (GlobalState, Widget) -> WidgetConfig -> StateT ExtendedState IO ()
drawDzenXft font input pos (gState, widget) wConf = do
  let cache = getIconCache gState
  cache2 <- liftIO $ withDraw $ \d -> do
    xftDrawSetClipRectangles d 0 0 [Rectangle (fi $ widgetX wConf) 0
                                    (fi $ widgetWidth wConf) (fi barHeight)]
    draw foregroundColorString backgroundColorString pos input d cache
  let gState2 = gState { getIconCache = cache2 }
  put (gState2, widget) where
    rs@(RenderState dpy w gc) = (getRenderState widget)
    scr = defaultScreen dpy
    vis = defaultVisual dpy scr
    colormap = defaultColormap dpy scr
    withDraw action = withXftDraw dpy w vis colormap action
    withColor color action = withXftColorName dpy vis colormap color action

    draw _ _ pos [] d ic = do
      let maxpos = (widgetX wConf) + (widgetWidth wConf)
      withColor backgroundColorString $ \c ->
        xftDrawRect d c pos 0 (maxpos - pos) barHeight
      return ic

    draw fg bg pos (Annotation Foreground fg' : xs) d ic = draw (withDefault foregroundColorString fg') bg pos xs d ic
    draw fg bg pos (Annotation Background bg' : xs) d ic = draw fg (withDefault backgroundColorString bg') pos xs d ic
    draw fg bg pos (IconRef winid : xs) d ic = do
      ((CachedIcon width height img), ic2) <- makeImage ic (fi winid)
      putImage dpy w gc img 0 0 (fi pos) 0 (fi width) (fi height)
      draw fg bg (pos + width) xs d ic2

    draw fg bg pos (Text msg : xs) d ic = do
      glyphInfo <- xftTextExtents dpy font msg
      withColor bg $ drawRect d glyphInfo
      withColor fg $ drawText d glyphInfo
      draw fg bg (pos + xglyphinfo_xOff glyphInfo) xs d ic where
        drawText d glyphInfo c = xftDrawString d c font (pos + xglyphinfo_x glyphInfo) 18 msg
        drawRect d glyphInfo c = xftDrawRect d c pos 0 (xglyphinfo_xOff glyphInfo) barHeight
