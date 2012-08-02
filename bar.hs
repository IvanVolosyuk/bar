-- Copyright 2012 Google Inc. All Rights Reserved.
-- Author: vol@google.com (Ivan Volosyuk)

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Font
import Graphics.X11.Xlib.Extras (changeProperty32, propModeReplace)
import GHC.Ptr (nullPtr)
import Data.Bits
import Foreign.C
import Graphics.X11.Xshape
import Control.Monad.State
import GHC.Conc (threadDelay)
import Numeric
import Text.Printf (printf)

padding = 4
barX = 0
barY = 768 - 24
barHeight = 24
barWidth = 1366
backgroundColor = 0xBEBEBE
cpuColorTable = [0x007F00, 0x7F0000, 0x600060, 0x0000FF]

height = fromIntegral barHeight

newtype IOBox a = IOBox { exec :: IO (a, IOBox a) }
type BoxIO a = IO (a, IOBox a)
box f = IOBox { exec = f }

data RenderState = RenderState { getDisplay :: Display, getWindow :: Window, getGC :: GC}
                                 
type MeasureFunc = RenderState -> IO Int
type RenderFunc = RenderState -> Int -> IO ()
data Renderer = Renderer { measure :: MeasureFunc, draw :: RenderFunc }

rectangles = [
  Rectangle 0 0 1366 wh
  ] where
      wh = fromIntegral barHeight

drawWindow rState w gc shape shape_gc = do
  let (RenderState dpy buf buf_gc) = rState
  setForeground dpy shape_gc 0
  fillRectangle dpy shape shape_gc 0 0 (fromIntegral barWidth) (fromIntegral barHeight) -- FIXME: do we need transparency mask at all?
  setForeground dpy shape_gc 1
  drawRectangles dpy shape shape_gc rectangles
  fillRectangles dpy shape shape_gc rectangles
  xshapeCombineMask dpy w shapeBounding 0 0 shape shapeSet

  setForeground dpy buf_gc backgroundColor
  drawRectangles dpy buf buf_gc rectangles
  fillRectangles dpy buf buf_gc rectangles

drawAllWidgets rState [] pos = return (rState, [])
drawAllWidgets rState widgets pos = do
  let widget : otherWidgets = widgets
  (rState, widget) <- execStateT (doUpdate widget) (rState,widget)
  (width, (rState, widget)) <- runStateT (onMeasure widget) (rState,widget)

  let (RenderState dpy w gc) = rState
  setForeground dpy gc 0x181838
  fillRectangle dpy w gc (fromIntegral $ pos - width - padding) 0 (fromIntegral $ width) (fromIntegral barHeight)
  setForeground dpy gc 0xFFFFFF

  (rState, widget) <- execStateT ((onDraw widget) (pos - width - padding)) (rState,widget) 
  (rState, otherWidgets) <- drawAllWidgets rState otherWidgets (pos - width - padding)
  return (rState, widget : otherWidgets)

eventLoop rState w gc shape shape_gc widgets = do
  drawWindow rState w gc shape shape_gc
  (rState, widgets) <- drawAllWidgets rState widgets 1366
  let RenderState dpy buf buf_gc = rState
  copyArea dpy buf w gc 0 0 barWidth barHeight 0 0
  sync dpy False
  threadDelay 100000
  nPending <- pending dpy
  if nPending == 0
    then do
      eventLoop rState w gc shape shape_gc widgets
    else do
      allocaXEvent $ \ev -> do
        nextEvent dpy ev
        evType <- get_EventType ev
        print $ "EventType: " ++ (show evType)
        if evType == buttonPress
          then return ()
          else do
            eventLoop rState w gc shape shape_gc widgets

main = do
  dpy <- openDisplay ":0" -- FIXME: proper way to get display name
  let scr = (defaultScreen dpy)
  let visual = defaultVisual dpy scr
  w <- createWindow dpy (defaultRootWindow dpy) barX barY barWidth barHeight
                    0 copyFromParent inputOutput visual 0 nullPtr
  let strutValues = [0, 0, 0, fromIntegral barHeight :: CLong,
                     0, 0, 0, 0,
                     0, 0, 0, 1360]
  strutPartial <- internAtom dpy "_NET_WM_STRUT_PARTIAL" False
  changeProperty32 dpy w strutPartial cARDINAL propModeReplace strutValues
  strut <- internAtom dpy "_NET_WM_STRUT" False
  changeProperty32 dpy w strut cARDINAL propModeReplace (take 4 strutValues)

  dockAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fromIntegral dockAtom]


  gc <- createGC dpy w
  setBackground dpy gc (whitePixel dpy scr) -- FIXME: figure out if this is needed

  buf <- createPixmap dpy w barWidth barHeight (defaultDepth dpy scr)
  buf_gc <- createGC dpy buf
  setBackground dpy gc backgroundColor
  setLineAttributes dpy gc 1 lineSolid capRound joinRound -- FIXME: use sane attributes for performance

  shape <- createPixmap dpy w barWidth barHeight 1
  shape_gc <- createGC dpy shape
  setBackground dpy gc (whitePixel dpy scr) -- FIXME: figure out if this is needed
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  selectInput dpy w (structureNotifyMask .|. buttonPressMask)
  mapWindow dpy w

  -- chan <- newChan :: IO (Chan (Int, String))
  cpuWidget <- makeCpuWidget defaultWidgetConfig { widgetWidth = 200 }

  let rState = RenderState dpy buf buf_gc --dpy w shape gc shape_gc
  eventLoop rState w gc shape shape_gc [staticMessageWidget, dynamicMessageWidget, cpuWidget]
  
  freePixmap dpy shape
  destroyWindow dpy w
  closeDisplay dpy


{-
registerWidget system name renderer = do
  let drWts = getDrawableWidgets system
  return system { getDrawableWidgets = M.insert name renderer (getDrawableWidgets system) }
  
unregisterWidget system name = return ()
          -- widget    width refresh rate
widgets = [
           (cpu, 70, 0.1)
          ]
-}
type ExtendedState = (RenderState, Widget)

data Widget = Widget {
    onMeasure :: StateT ExtendedState IO Int,
    onDraw :: Int -> StateT ExtendedState IO (),
    doUpdate :: StateT ExtendedState IO (),
    onMouseEnter :: StateT ExtendedState IO (),
    onMouseLeave :: StateT ExtendedState IO (),
    onMouseClick :: StateT ExtendedState IO (),
    widgetConfig :: WidgetConfig
}

data WidgetFunctions st = WidgetFunctions {
    onMeasureFunc :: st -> StateT ExtendedState IO Int,
    onDrawFunc :: st -> Int -> StateT ExtendedState IO (),
    doUpdateFunc :: st -> StateT ExtendedState IO (),
    onMouseEnterFunc :: st -> StateT ExtendedState IO (),
    onMouseLeaveFunc :: st -> StateT ExtendedState IO (),
    onMouseClickFunc :: st -> StateT ExtendedState IO ()
}

defaultFunctions = WidgetFunctions {
  onMeasureFunc = simpleOnMeasure,
  onDrawFunc = \_ _ -> return (),
  doUpdateFunc = \_ -> return (),
  onMouseEnterFunc = \_ -> return (),
  onMouseLeaveFunc = \_ -> return (),
  onMouseClickFunc = \_ -> return ()
  }

data WidgetConfig = WidgetConfig { widgetWidth :: Int }
defaultWidgetConfig = WidgetConfig { widgetWidth = 100 }

data CpuState = CpuState { samples :: [[Int]] }


simpleOnMeasure :: a -> StateT ExtendedState IO Int
simpleOnMeasure _ = do
  (_,widgetState) <- get
  return . widgetWidth . widgetConfig $ widgetState

staticMessageDraw :: String -> () -> Int -> StateT ExtendedState IO ()
staticMessageDraw text _ pos = do
  (renderState,widget) <- get
  let (RenderState dpy w gc) = renderState
  liftIO $ drawString dpy w gc (fromIntegral pos) 10 text



wrapWidget functions widgetConfig state =
  let WidgetFunctions onM onD doU onME onML onMC = functions in
  Widget { onMeasure = onM state,
           onDraw = onD state,
           doUpdate = doU state,
           onMouseEnter = onME state,
           onMouseLeave = onML state,
           onMouseClick = onMC state,
           widgetConfig = widgetConfig }

staticMessageWidget = wrapWidget defaultFunctions { onDrawFunc = staticMessageDraw "static message" } defaultWidgetConfig ()


dynamicMessageDraw :: Int -> Int -> StateT ExtendedState IO ()
dynamicMessageDraw count pos = do
  (renderState,widget) <- get
  let (RenderState dpy w gc) = renderState
  liftIO $ drawString dpy w gc (fromIntegral pos) 10 $ printf "Frame: %d" count
  let widget2 = wrapWidget dynamicWidgetFunctions (widgetConfig widget) (count+1)
  put (renderState, widget2)


-- TODO list
-- 1. Make config values usable
-- 2. Move generation of data into separate threads to make it fault tolerant (?)
--     Have one master channel which notifies about events the master thread (?)
--     Have lots of secondary channels which feed data into renderers
  

dynamicWidgetFunctions = defaultFunctions { onDrawFunc = dynamicMessageDraw }

dynamicMessageWidget = wrapWidget dynamicWidgetFunctions defaultWidgetConfig 0


makeSegment (x,y) = Segment x' y0' x' y1' where
  x' = fromIntegral x
  y0' = fromIntegral height
  y1' = (fromIntegral height) - (fromIntegral y)

drawColorSegments dpy w gc [] = return ()
drawColorSegments dpy w gc (x:xs) = do
  let (segments, color) = x
  setForeground dpy gc color
  drawSegments dpy w gc segments
  drawColorSegments dpy w gc xs

getCpuData = readFile "/proc/stat" >>= return . map(read) . words . head . lines
delta newar ar = map (\(n,o) -> n-o) $ zip newar ar
updateGraph samples sample = newSamples where
  newSamples = map (\(n,o) -> o++[n]) $ zip sample $ map (drop 1) samples
makeCpuSample :: [Int] -> [Dimension]
makeCpuSample (_ :user:nice:sys:idle:io:tail) = map (makeLine total) values where
  (total:values) = reverse $ accumulate 0 [sys + io, nice, user, idle]
accumulate = scanl (+)
safe total = if total /= 0 then total else 1
makeLine total = fromIntegral . (`div` safe total) . (* height)

{-graphRenderer colorTable cmd graph = Renderer { measure = m, draw = d} where
  m _ = return . length . head $ graph
  d rState x = do
    let segments = map (\a -> map makeSegment $ zip [x..] a) graph
    let (RenderState dpy w gc) = rState
    drawColorSegments dpy w gc $ zip segments colorTable
    fn <- fontFromGC dpy gc
    fs <- queryFont dpy fn
    let sz = textWidth fs "Hello"
    drawString dpy w gc 10 10 "Hello"
    return ()

cpu width = do
  let zeroGraph = take 3 $ repeat $ take width $ repeat 0
  procData <- getCpuData
  cpu' width zeroGraph procData where
    cpu' width graph procData = do
      newProcData <- liftIO $ getCpuData
      let procDelta = delta newProcData procData
          newGraph = updateGraph graph $ makeCpuSample procDelta
      return (graphRenderer cpuColorTable "top.sh" newGraph,
              IOBox { exec = cpu' width newGraph newProcData })-}


makeCpuWidget :: WidgetConfig -> IO Widget
makeCpuWidget config = do
  let zeroGraph = take 3 $ repeat $ take (widgetWidth config) $ repeat 0
  procData <- getCpuData
  let state = (procData, zeroGraph)
  return $ wrapWidget functions config state where
    functions = defaultFunctions { onDrawFunc = draw, doUpdateFunc = update }
    draw (_,graph) pos = do
      (rState, _) <- get
      let segments = map (\a -> map makeSegment $ zip [pos..] a) graph
      let (RenderState dpy w gc) = rState
      liftIO $ drawColorSegments dpy w gc $ zip segments cpuColorTable
    update state = do
      (rState, widget) <- get
      let (procData,graph) = state
      newProcData <- liftIO $ getCpuData
      let procDelta = delta newProcData procData
          newGraph = updateGraph graph $ makeCpuSample procDelta
      put (rState, wrapWidget functions (widgetConfig widget) (newProcData, newGraph))
      
      
