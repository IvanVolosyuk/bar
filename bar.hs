-- Copyright 2012 Google Inc. All Rights Reserved.
-- Author: vol@google.com (Ivan Volosyuk)

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Event
import Graphics.X11.Xlib.Extras (changeProperty32, propModeReplace)
import GHC.Ptr (nullPtr)
import Data.Bits
import Foreign.C
import Graphics.X11.Xshape

newtype IOBox a = IOBox { unbox :: IO (a, IOBox a) }
type BoxIO a = IO (a, IOBox a)
box f = IOBox { unbox = f }

data RenderState = RenderState
type MeasureFunc = RenderState -> IO Int
type RenderFunc = RenderState -> Int -> IO ()
data Renderer = Renderer { measure :: MeasureFunc, draw :: RenderFunc }

textRenderer text = Renderer { measure = m, draw = d text } where
  m _ = return 5
  d text _ _ = print text

twoMsgs2 = do
  initialMsg <- getLine
  genTitle' initialMsg where
    genTitle' oldMsg = do
      newMsg <- getLine
      let output = "newMsg: " ++ newMsg ++ " oldMsg: " ++ oldMsg
      return (textRenderer output, box $ genTitle' newMsg) where

loop inbox = do
  (output, outbox) <- unbox inbox
  sz <- (measure output) RenderState
  print $ "sz: " ++ (show sz)
  (draw output) RenderState 1024
  loop outbox

main2 = do
  (output, box) <- twoMsgs2
  sz <- (measure output) RenderState
  print $ "sz: " ++ (show sz)
  (draw output) RenderState 1024
  loop box


barX = 0
barY = 0
barHeight = 24
barWidth = 1024

rectangles = [
  Rectangle 0 0 rw rh,
  Rectangle 0 w rw rh
  ] where
      rw = fromIntegral 100
      rh = fromIntegral 6
      wh = fromIntegral barHeight
      w = fromIntegral $ wh-rh

drawWindow dpy w pmap gc shape_gc = do
  setForeground dpy shape_gc 0
  fillRectangle dpy pmap shape_gc 0 0 (fromIntegral barWidth) (fromIntegral barHeight)
  setForeground dpy shape_gc 1
  drawRectangles dpy pmap shape_gc rectangles
  fillRectangles dpy pmap shape_gc rectangles
  xshapeCombineMask dpy w shapeBounding 0 0 pmap shapeSet

  setForeground dpy shape_gc (whitePixel dpy (defaultScreen dpy))
  drawRectangles dpy w gc rectangles
  fillRectangles dpy w gc rectangles
  sync dpy False

eventLoop dpy w pmap gc shape_gc = do
  allocaXEvent $ \ev -> do
    nextEvent dpy ev
    evType <- get_EventType ev
    print $ "EventType: " ++ (show evType)
    if evType == buttonPress
      then return ()
      else do
        drawWindow dpy w pmap gc shape_gc
        eventLoop dpy w pmap gc shape_gc

main = do
  dpy <- openDisplay ":0" -- FIXME: proper way to get display name
  let visual = defaultVisual dpy (defaultScreen dpy)
  w <- createWindow dpy (defaultRootWindow dpy) barX barY barWidth barHeight
                    0 copyFromParent inputOutput visual 0 nullPtr
  let strutValues = [0, 0, fromIntegral barHeight :: CLong, 0,
                     0, 0, 0, 0,
                     0, 1360, 0, 0]
  strutPartial <- internAtom dpy "_NET_WM_STRUT_PARTIAL" False
  changeProperty32 dpy w strutPartial cARDINAL propModeReplace strutValues
  strut <- internAtom dpy "_NET_WM_STRUT" False
  changeProperty32 dpy w strut cARDINAL propModeReplace (take 4 strutValues)

  dockAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fromIntegral dockAtom]


  gc <- createGC dpy w
  setBackground dpy gc (whitePixel dpy (defaultScreen dpy)) -- FIXME: figure out if this is needed
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  pmap <- createPixmap dpy w barWidth barHeight 1
  shape_gc <- createGC dpy pmap
  setBackground dpy gc (whitePixel dpy (defaultScreen dpy)) -- FIXME: figure out if this is needed
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  selectInput dpy w (structureNotifyMask .|. buttonPressMask)
  mapWindow dpy w

  drawWindow dpy w pmap gc shape_gc
  eventLoop dpy w pmap gc shape_gc
