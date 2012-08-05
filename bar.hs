-- Copyright 2012 Google Inc. All Rights Reserved.
-- Author: vol@google.com (Ivan Volosyuk)

import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.State
import Data.Bits
import Data.Time
import Foreign.C
import GHC.Ptr (nullPtr)
import Graphics.X11.Xft
import Graphics.X11.Xrender
import Graphics.X11.Xshape
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
barY = 768 - 24 - 1
barHeight = 24 :: Int
barWidth = 1366 :: Int
backgroundColor = 0xBEBEBE
foregroundColor = 0x000000
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
   (makeNetWidget "wlan0", 40, 0.1, wc),
   (makeTitleWidget, 1028, 0, wc { drawFrame = False })
   ]

height = barHeight - marginsVertical :: Int

newtype IOBox a = IOBox { exec :: BoxIO a }
type BoxIO a = IO (a, IOBox a)

type ControlChan = Chan CInt

data RenderState = RenderState { getDisplay :: Display, getDrawable::Pixmap, getGC :: GC}
data WindowRenderState = WindowRenderState Window GC Pixmap GC
data GlobalState = GlobalState { getRenderState :: RenderState,
                                 getWindowRenderState :: WindowRenderState,
                                 getIconCache :: IconCache,
                                 getFont :: Either XftFont FontSet,
                                 widgetsById :: M.Map CInt Widget,
                                 widgetsToRedraw :: S.Set CInt
                               }
                                 
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

drawWindow rs (WindowRenderState w gc shape shape_gc) = do
  {- Shape mask, we don't probably need that
  let shape_rs = RenderState (getDisplay rs) shape shape_gc
  setfg shape_rs 0
  fillRect shape_rs 0 0 barWidth barHeight -- FIXME: do we need transparency mask at all?
  setfg shape_rs 1
  fillRect shape_rs 0 0 barWidth barHeight
  xshapeCombineMask (getDisplay rs) w shapeBounding 0 0 shape shapeSet -}

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

  let rs = (getRenderState gState)
  setfg rs graphBackgroundColor
  fillRect rs widgetPos marginTop width (barHeight - marginsVertical)
  setfg rs 0xFFFFFF

  (gState, widget2) <- execStateT ((onDraw widget2) widgetPos) (gState,widget2) 
  (gState, otherWidgets) <- drawAllWidgets gState otherWidgets (widgetPos)
  return (gState, (widgetId,widget2) : otherWidgets)

data EventResult = Done | ExitEventLoop | Update GlobalState

updateWindow = updateWindowRegion 0 barWidth

updateWindowRegion x width gState = do
  let RenderState dpy buf buf_gc = (getRenderState gState)
  let WindowRenderState w gc _ _ = (getWindowRenderState gState)
  copyArea dpy buf w gc x 0 (fi width) (fi barHeight)
                            (fi x) 0
  sync dpy False

handleMessage _ (ClientMessageEvent {ev_data = (-1):_}) = return ExitEventLoop
handleMessage gState (ClientMessageEvent {ev_data = widgetId:_}) = do
  -- print $ "Update for widget: " ++ (show widgetId)
  let widgets = widgetsById gState
  case M.lookup widgetId widgets of
    Nothing -> return Done
    Just widget -> do
      (gState, widget) <- execStateT (doUpdate widget) (gState,widget)

      let x = widgetX . widgetConfig $  widget
      let wConf = widgetConfig widget
      let width = widgetWidth wConf
      when (drawFrame wConf) $ do
        let rs = (getRenderState gState)
        setfg rs graphBackgroundColor -- FIXME: move to widget draw?
        fillRect rs x marginTop width (barHeight - marginsVertical)

      (gState, widget) <- execStateT ((onDraw widget) x) (gState, widget) 
      updateWindowRegion (fi x) (fi width) gState
      return . Update $ gState { widgetsById = M.insert widgetId widget widgets }
  
handleMessage gState (ButtonEvent {ev_x = x}) = do
  return ExitEventLoop

handleMessage gState (ExposeEvent {}) = do
  drawWindow (getRenderState gState) (getWindowRenderState gState)
  let widgets = M.toList . widgetsById $ gState
  (gState, widgets) <- drawAllWidgets gState widgets (fi barWidth)
  print "MapNotify: expensive redraw all"
  updateWindow gState
  return $ Update gState { widgetsById = M.fromList widgets }

handleMessage gState (MotionEvent {}) = return Done

handleMessage gState (AnyEvent {ev_event_type = 14}) = return Done -- NoExpose

handleMessage _ event = (print $ "Unhandled event:" ++ (eventName event) ++ ": " ++ (show event)) >> return Done

eventMap = M.fromList eventTable

eventLoop gState = do
  let dpy = getDisplay . getRenderState $ gState
  allocaXEvent $ \ev -> do
    nextEvent dpy ev
    event <- getEvent ev
    -- print $ show $ event
    mbrState <- handleMessage gState event
    case mbrState of
      ExitEventLoop -> return ()
      Done -> eventLoop gState
      Update gState' -> eventLoop gState'

sendClientEvent d a w val = do
    allocaXEvent $ \e -> do
         setEventType e clientMessage
         setClientMessageEvent e w a 32 val currentTime
         sendEvent d w False structureNotifyMask e
    sync d False

copyChanToX chan w = do
  d   <- openDisplay ""
  a <- internAtom d "BAR_UPDATE" False
  forever $ do
     x <- readChan chan
     -- print $ "Copy to X: " ++ (show x)
     sendClientEvent d a w (fi x)

inputReader chan = loopForever `catch` \x -> writeChan chan (-1) where
  loopForever = forever $ do
    line <- getLine
    print $ "Line: " ++ line
    writeChan chan 333

main = do
  dpy <- openDisplay "" -- FIXME: proper way to get display name
  let scr = (defaultScreen dpy)
  let visual = defaultVisual dpy scr
  w <- createWindow dpy (defaultRootWindow dpy) (fi barX) (fi barY)
                        (fi barWidth) (fi barHeight)
                    0 copyFromParent inputOutput visual 0 nullPtr
  let strutValues = [0, 0, 0, fi barHeight :: CLong,
                     0, 0, 0, 0,
                     0, 0, 0, 1360]
  strutPartial <- internAtom dpy "_NET_WM_STRUT_PARTIAL" False
  changeProperty32 dpy w strutPartial cARDINAL propModeReplace strutValues
  strut <- internAtom dpy "_NET_WM_STRUT" False
  changeProperty32 dpy w strut cARDINAL propModeReplace (take 4 strutValues)

  dockAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi dockAtom]


  gc <- createGC dpy w
  setBackground dpy gc (whitePixel dpy scr) -- FIXME: figure out if this is needed

  buf <- createPixmap dpy w (fi barWidth) (fi barHeight) (defaultDepth dpy scr)
  buf_gc <- createGC dpy buf
  setBackground dpy gc backgroundColor
  setLineAttributes dpy gc 1 lineSolid capRound joinRound -- FIXME: use sane attributes for performance

  shape <- createPixmap dpy w (fi barWidth) (fi barHeight) 1
  shape_gc <- createGC dpy shape
  setBackground dpy gc (whitePixel dpy scr) -- FIXME: figure out if this is needed
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  selectInput dpy w (structureNotifyMask .|. buttonPressMask 
                 .|. enterWindowMask .|. leaveWindowMask .|. pointerMotionMask
                 .|. exposureMask)
  mapWindow dpy w

  chan <- newChan
  t1 <- forkOS $ copyChanToX chan w

  widgets <- mapM (initWidget chan) $ zip [1..] loadWidgets

  let rState = RenderState dpy buf buf_gc --dpy w shape gc shape_gc
  let wrState = WindowRenderState w gc shape shape_gc
  font <- case fontXft of
    False -> createFontSet dpy fontName >>= \(_,_,fontSet) -> return $ Right fontSet
    True -> xftFontOpenXlfd dpy (defaultScreenOfDisplay dpy) fontName >>= return . Left
  iconCache <- makeIconCache dpy
  let gState = GlobalState rState wrState iconCache font (M.fromList widgets) S.empty
  eventLoop gState
  
  killThread t1
  mapM (destroyWidget gState) (M.toList . widgetsById $ gState)
  freePixmap dpy shape
  destroyWindow dpy w
  closeDisplay dpy

destroyWidget gState (_,w) = runStateT (onDestroy w) (gState, w)

initWidget :: ControlChan -> (CInt, (MakeWidget, Int, Double, WidgetConfig)) -> IO (CInt, Widget)
initWidget chan (widgetId, (makeWidget, width, refresh, config)) = do
  widget <- makeWidget chan config {
                widgetWidth = width,
                refreshRate = (truncate $ refresh * 1000000),
                widgetId = widgetId }
  return (widgetId, widget)


type ExtendedState = (GlobalState, Widget)

data Widget = Widget {
    onMeasure :: StateT ExtendedState IO Int,
    onDraw :: Int -> StateT ExtendedState IO (),
    doUpdate :: StateT ExtendedState IO (),
    onDestroy :: StateT ExtendedState IO (),
    widgetConfig :: WidgetConfig
}
type MakeWidget = ControlChan -> WidgetConfig -> IO Widget

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

data WidgetConfig = WidgetConfig { widgetWidth :: Int, widgetId :: CInt, refreshRate :: Int, widgetX :: Int, drawFrame :: Bool }
defaultWidgetConfig = WidgetConfig { widgetWidth = 100, widgetId = 0, refreshRate = 100000, widgetX = 0, drawFrame = True }

data CpuState = CpuState { samples :: [[Int]] }


simpleOnMeasure :: a -> StateT ExtendedState IO Int
simpleOnMeasure _ = do
  (_,widgetState) <- get
  return . widgetWidth . widgetConfig $ widgetState

staticMessageDraw :: String -> () -> Int -> StateT ExtendedState IO ()
staticMessageDraw text _ pos = do
  (gState,widget) <- get
  let (RenderState dpy w gc) = (getRenderState gState)
  liftIO $ drawString dpy w gc (fi pos) 10 text


wrapWidget functions widgetConfig state =
  let WidgetFunctions onM onD doU onDe = functions in
  Widget { onMeasure = onM state,
           onDraw = onD state,
           doUpdate = doU state,
           onDestroy = onDe state,
           widgetConfig = widgetConfig }

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


runBackgroundTask :: ControlChan -> WidgetConfig -> BoxIO a -> IO (Chan a, ThreadId)
runBackgroundTask masterChan config boxio = do
  clientChan <- newChan -- FIXME: chan get rid of clientChan parameter?
  threadId <- forkOS $ thread clientChan boxio
  return (clientChan, threadId) where
    refresh = refreshRate config
    n = widgetId config
    thread clientChan boxio = do
      (output, box2) <- boxio
      -- print $ "bgTask sends: " ++ (show n)
      writeChan masterChan n
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

makeWidgetWithThread makeInitialState makeThread prepareData drawData masterChan config = do
  let initialState = makeInitialState config
  (clientChan, threadId) <- runBackgroundTask masterChan config makeThread
  let state = (initialState, clientChan, threadId)
  return $ wrapWidget functions config state where
    functions = defaultFunctions { onDrawFunc = draw, doUpdateFunc = update, onDestroyFunc = destroy }
    draw (widgetState,_,_) pos = drawData widgetState pos
    update state = do
      (gState, widget) <- get
      let (widgetState, chan, threadId) = state
      newSample <- liftIO $ readChan chan
      let newWidgetState = prepareData widgetState newSample
      put (gState, wrapWidget functions (widgetConfig widget) (newWidgetState, chan, threadId))
    destroy (_,_,threadId) = liftIO $ killThread threadId

dropZeros [] = []
dropZeros ((i,0):xs) = dropZeros xs
dropZeros ((i,v):xs) = (i,v) : dropZeros xs

makeGraphWidget colorTable makeThread =
  makeWidgetWithThread initialState makeThread prepareData drawData where
    initialState config = replicate (length colorTable) $ replicate (widgetWidth config) 0
    prepareData state newSample = updateGraph state newSample
    drawData graph pos = do
      (gState, _) <- get
      let segments = map (\a -> map makeSegment $ dropZeros $ zip [pos..] a) graph -- FIXME: segments using x position
      -- liftIO . print . show . map head . map reverse $ segments
      let rs = getRenderState gState
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
makeNetWidget dev masterChan config = makeGraphWidget netColorTable
                                      makeThread masterChan config where
  makeThread = makeBox (liftIO $ readNetFile "/proc/net/dev") $ \netState -> do
    newNetState <- liftIO $ readNetFile "/proc/net/dev"
    let netDelta = delta (newNetState ! dev) (netState ! dev)
    -- print $ show $ makeNetSample netDelta
    return ((makeNetSample netDelta), newNetState)
  f x = log (x + 1)
  f2 x = (f x) * (f x)
  maxF = (*2) . f2 . (*10) . fi $ (refreshRate config) -- max rate 10 Mb/s
  makeNetSample input = map (makeLine total) values where
    atIndex idx = f2 . fi $ input !! idx
    inbound = atIndex 0
    outbound = atIndex 8
    total = truncate maxF
    values = map truncate [maxF, maxF - inbound, outbound] :: [Int]

drawText msg pos = do
  (gState, _) <- get
  let rs@(RenderState dpy w gc) = (getRenderState gState)
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
    initialState config = [ IconRef 0x300002b ] -- FIXME
    prepareData state newMsg = newMsg
    drawData = drawDzen
    exit _ = writeChan masterChan (-1) >> return ""
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
    draw "#000000" "#BEBEBE" pos input d cache
  let gState2 = gState { getIconCache = cache2 }
  put (gState2, widget) where
    rs@(RenderState dpy w gc) = (getRenderState gState)
    scr = defaultScreen dpy
    vis = defaultVisual dpy scr
    colormap = defaultColormap dpy scr
    withDraw action = withXftDraw dpy w vis colormap action
    withColor color action = withXftColorName dpy vis colormap color action

    draw _ _ pos [] d ic = do
      let maxpos = (widgetX wConf) + (widgetWidth wConf)
      withColor "#BEBEBE" $ \c ->
        xftDrawRect d c pos 0 (maxpos - pos) barHeight
      return ic

    draw fg bg pos (Annotation Foreground fg' : xs) d ic = draw (withDefault "#000000" fg') bg pos xs d ic
    draw fg bg pos (Annotation Background bg' : xs) d ic = draw fg (withDefault "#BEBEBE" bg') pos xs d ic
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
