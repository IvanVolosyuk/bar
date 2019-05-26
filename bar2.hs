{-# LANGUAGE Arrows #-}

import Control.Applicative
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Collection
import Control.Auto.Core
import Control.Auto.Effects
import Control.Auto.Time
import Control.Concurrent
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.IORef
import Data.List
import Data.Map ((!))
import Data.Maybe
import Data.Time
import Data.Time.Zones
import Data.Word
import Foreign.Ptr
import Graphics.X11.Xft
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Color
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xrender
import Numeric
import System.IO.Error
import Text.Printf

import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Set as S
import Prelude hiding (id, (.))

import DzenParse
import Icon
import Timer
import Top
import Utils

barHeight :: Int
barHeight = 24

marginTop :: Int
marginTop = 1

marginBottom :: Int
marginBottom = 1

barBackground :: String
barBackground = "#BEBEBE"

infoBackground :: String
infoBackground = "#181838"

tooltipBackground :: String
tooltipBackground = "#FFFFC0"

bars :: [Bar]
--bars = [bar1, bar2]
bars = [bar1]

bar1a = Bar barBackground barHeight {-screen-} 0 GravityBottom [
  label # Message "foo" # Width 50 # memTooltip2
                                                ]
bar1 :: Bar
bar1 = Bar barBackground barHeight {-screen-} 0 GravityBottom [
        clock # TimeFormat "%R:%S" #
            Width 90 # RightPadding 4 #
            LocalTimeZone # BackgroundColor infoBackground #
            clockTooltip,
        logtm cpu # cpuTooltip2,
        label # Message "foo" # Width 50,
        logtm mem # memTooltip2,
        logtm (net "brkvm"),

        title # LeftPadding 2 # RightPadding 2 #
                BackgroundColor barBackground #
                JustifyLeft # TextColor "#000000"
      ]

bar2 :: Bar
bar2 = Bar barBackground (barHeight*2) {-screen-} 0 GravityTop [
        clock # TimeFormat "%R" # 
            Width 60 # RightPadding 4 #
            LocalTimeZone # BackgroundColor infoBackground #
            clockTooltip,

        title # LeftPadding 2 # RightPadding 2 #
                BackgroundColor barBackground #
                JustifyLeft # TextColor "#000000"
      ]

clockTooltip = Tooltip tooltipBackground (Size 460 (barHeight * 2)) Horizontal [
        frame Vertical [
                         tooltipClock #OtherTimeZone "America/Los_Angeles",
                         tooltipClock
                       ] #Width 340,
        frame Vertical [
           tooltipLabel #Message "MTV: " #JustifyRight,
           tooltipLabel #Message "Local: " #JustifyRight
                       ]
 ]

cpuTooltip = Tooltip tooltipBackground (Size 300 (6*barHeight)) Horizontal [
     tooltipText cpuTop #Width 280,
     tooltip cpu #RefreshRate 0.1 # Width 20 #LinearTime
     ]

cpuTooltip2 = Tooltip tooltipBackground (Size 300 (6*barHeight)) Horizontal [
     tooltip cpu # RefreshRate 0.05 # Width 300
     ]

memTooltip = Tooltip tooltipBackground (Size 450 (6*barHeight)) Horizontal [
     tooltipText memstatus #Width 430 #LeftPadding 5,
     tooltip mem #RefreshRate 0.1 # Width 20 #LinearTime
     ]

memTooltip2 = Tooltip tooltipBackground (Size 450 (6*barHeight)) Horizontal [
     tooltip mem #RefreshRate 1 # LinearTime
     ]

netTooltip netdev = Tooltip tooltipBackground (Size 400 (2*barHeight)) Horizontal [
     tooltipText (netstatus netdev) #RefreshRate 3 # Width 380 #JustifyLeft #LeftPadding 10,
     tooltip (tooltipNet netdev) #RefreshRate 0.1 #LinearTime # Width 20
     ]

logtm w = w # LogTime 8 # Width 129 # RefreshRate 0.05 -- One week worth of data

tooltip w = w #BackgroundColor "#FFFFC0"
              #TopPadding 0 #BottomPadding 1 #LeftPadding 0 #RightPadding 1

tooltipText w = tooltip w  #TextColor "#000000"
              #SetFont "-*-courier new-*-r-normal-*-17-*-*-*-*-*-*-*"

tooltipClock = tooltipText clock #TimeFormat "%a, %e %b %Y - %X"
tooltipLabel = tooltipText label
tooltipNet netdev = Graph defaultAttr (GraphDef (Net netdev) (LogTime 8) 1)
                    ["#6060FF", tooltipBackground, "#60FF60"]  # Width 129

data Attribute = Width Int | Height Int | LeftPadding Int | RightPadding Int
               | TopPadding Int | BottomPadding Int
               | TextColor Main.Color | BackgroundColor Main.Color
               | TimeFormat String | Message String | SetFont String
               | RefreshRate Period

type Color = String
type Font = String
type Pos = Size

type OnClickCmd = String

data Gravity = GravityTop | GravityBottom deriving (Show, Eq)
data Orientation = Horizontal | Vertical deriving (Show, Eq)

data ClockTimeZone = LocalTimeZone | OtherTimeZone String deriving (Show, Eq)
data Justify = JustifyLeft | JustifyMiddle | JustifyRight deriving (Show, Eq)
data Padding = Padding Size Size deriving (Show, Eq)
data TextAttributes = TextAttributes Main.Color Justify Main.Font Int deriving (Show, Eq)
data WidgetAttributes = WidgetAttributes {
  size :: Size,
  position :: Pos,
  padding :: Padding,
  color :: Main.Color,
  onclick :: Maybe OnClickCmd,
  mbtooltip :: Maybe Tooltip } deriving (Show, Eq)

-- int = n linear points before 2x compression
data GraphType = Cpu | Net String | Mem | Battery deriving (Show,Eq,Ord)
data TimeScale = LinearTime | LogTime Int deriving (Show,Eq,Ord)
data GraphDef = GraphDef { type_ :: GraphType, tscale_ :: TimeScale, period_ :: Period} deriving (Show, Eq, Ord)

data Bar = Bar String Int ScreenNumber Gravity [Widget] deriving Show
data Tooltip = Tooltip String Size Orientation [Widget] deriving (Show, Eq)

data Widget = Clock   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, fmt_ :: String, tz_ :: ClockTimeZone, refreshRate :: Period }
          | Label   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, label_ ::  String }
          | Title   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes }
          | CpuTop  {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate :: Period}
          | NetStatus{attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate :: Period, netdev_ :: String}
          | MemStatus{attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate :: Period}
          | Frame   {attr_ :: WidgetAttributes, orient_ :: Orientation, children_ :: [Widget]}
          | Graph   {attr_ :: WidgetAttributes, graph_ :: GraphDef, colorTable :: [String]}
          deriving (Show, Eq)

defaultAttr :: WidgetAttributes
defaultAttr = WidgetAttributes (Size 400 barHeight) 0 (Padding 1 1) infoBackground Nothing Nothing

defaultTAttr :: TextAttributes
defaultTAttr = TextAttributes "#C7AE86" JustifyMiddle "-*-*-medium-r-normal--15-*-*-*-*-*-iso10646-*" barHeight

clock :: Widget
clock = Clock defaultAttr defaultTAttr "%R" LocalTimeZone 1

cpu :: Widget
cpu = Graph defaultAttr (GraphDef Cpu (LogTime 8) 1) ["#70FF70", "#FF8080", "#F020F0", "#3030FF"] -- # Width 129

mem :: Widget
mem = Graph defaultAttr (GraphDef Mem (LogTime 8) 1) ["#00FF00", "#6060FF"] -- # Width 129

net :: String -> Widget
net netdev = Graph defaultAttr (GraphDef (Net netdev) (LogTime 8) 1)
             ["#6060FF", infoBackground, "#60FF60"] # Width 129 #netTooltip netdev

netstatus :: String -> Widget
netstatus = NetStatus defaultAttr defaultTAttr 1

memstatus :: Widget
memstatus = MemStatus defaultAttr defaultTAttr 1 #JustifyLeft

label :: Widget
label = Label defaultAttr defaultTAttr ""

title :: Widget
title = Title defaultAttr defaultTAttr # Width 4000

cpuTop :: Widget
cpuTop = CpuTop defaultAttr defaultTAttr 3 # JustifyLeft

frame :: Orientation -> [Widget] -> Widget
frame = Frame (WidgetAttributes (Size 5000 barHeight) 0 (Padding 0 0)
                               "#181838" Nothing Nothing)

class Apply a where
  apply :: a -> Widget -> Widget

instance Apply ClockTimeZone where
  apply tz ww = ww { tz_ = tz }

instance Apply Justify where
  apply j ww = let TextAttributes c _ f hs = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}

instance Apply TimeScale where
  apply t ww = let g = graph_ ww in ww { graph_ = g { tscale_ = t } }

instance Apply Tooltip where
  apply tip ww = let WidgetAttributes ws x p c cmd _ = attr_ ww
                   in ww { attr_ = WidgetAttributes ws x p c cmd (Just tip) }

withAttr ww f = ww { attr_ = f (attr_ ww) }
withPadding ww f = withAttr ww $ \wa -> wa { padding = f (padding wa) }

instance Apply Attribute where
  apply (TextColor c) ww = let TextAttributes _ j f hs = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}
  apply (SetFont f) ww = let TextAttributes c j _ hs = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}

  apply (Width w) ww = withAttr ww $ \wa -> wa { size = Size w (y_ . size $ wa)}
  apply (Height h) ww = withAttr ww $ \wa -> wa { size = Size (x_ . size $ wa) h}

  apply (LeftPadding l) ww = withPadding ww $ \p -> let Padding (Size _ t) pbr = p in Padding (Size l t) pbr
  apply (TopPadding t) ww = withPadding ww $ \p -> let Padding (Size l _) pbr = p in Padding (Size l t) pbr
  apply (RightPadding r) ww = withPadding ww $ \p -> let Padding plt (Size _ b) = p in Padding plt (Size r b)
  apply (BottomPadding b) ww = withPadding ww $ \p -> let Padding plt (Size r _) = p in Padding plt (Size r b)
  apply (BackgroundColor c) ww = withAttr ww $ \attr -> let WidgetAttributes ws x p _ cmd tip = attr in
                                                     WidgetAttributes ws x p c cmd tip
  apply (TimeFormat fmt) ww = ww { fmt_ = fmt }
  apply (Message s) ww = ww { label_ = s }
  apply (RefreshRate r) ww@(Graph _ def _) = ww { graph_ = def {period_ = r }}
  apply (RefreshRate r) ww = ww { refreshRate = r }

instance Num Size where
  (+) (Size x0 y0) (Size x1 y1) = Size (x0 + x1) (y0 + y1)
  (-) (Size x0 y0) (Size x1 y1) = Size (x0 - x1) (y0 - y1)
  (*) (Size x0 y0) (Size x1 y1) = Size (x0 * x1) (y0 * y1)
  abs (Size x y) = Size (abs x) (abs y)
  signum (Size x y) = Size (signum x) (signum y)
  fromInteger a = Size (fi a) (fi a)
half (Size w h) = Size (w `div` 2) (h `div` 2)

infixl 9 #
(#) :: (Apply a) => Widget -> a  -> Widget
(#) w a = apply a w

makeFont :: RenderState -> TextAttributes -> IO XftFont
makeFont RenderState { display = dpy } (TextAttributes _ _ fontName _) =
  xftFontOpenXlfd dpy (defaultScreenOfDisplay dpy) fontName

localTimezone :: IO (UTCTime -> IO ZonedTime)
localTimezone = do
  timezone <- getCurrentTimeZone
  return $ \utcTime -> do
    let localTime = utcToLocalTime timezone utcTime
    return $ ZonedTime localTime timezone

otherTimezone :: String -> IO (UTCTime -> IO ZonedTime)
otherTimezone timezone = do
  tz <- loadSystemTZ timezone
  return $ \utcTime -> do
    let timeZone = timeZoneForUTCTime tz utcTime
    let localTime = utcToLocalTime timeZone utcTime
    return $ ZonedTime localTime timeZone

formatClock :: String -> (UTCTime -> IO ZonedTime) -> IO String
formatClock fmt zoned = do
  time <- getCurrentTime
  zonedTime <- zoned time
  return $ formatTime Data.Time.defaultTimeLocale fmt zonedTime



type DrawCall a = StateT IconCache IO a
type DrawCallback = Maybe XftDraw -> DrawCall ()
type DrawRef = IORef DrawCallback
type ControlChan = Chan RootInput

data RenderState = RenderState {
  display :: Display,
  window :: Window,
  buffer :: Pixmap,
  gc_ :: GC,
  windowWidth :: Int,
  windowHeight :: Int,
  windowBackground :: String,
  screenNum :: ScreenNumber
}

data WindowState = WindowState RenderState [Widget]

instance Show RenderState where
  show RenderState {} = "RenderState "

visualColormap :: Display -> (Visual, Colormap)
visualColormap dpy = (vis, colormap) where
  scr = defaultScreen dpy
  vis = defaultVisual dpy scr
  colormap = defaultColormap dpy scr

withDraw :: RenderState  -> (XftDraw -> IO a) -> IO a
withDraw  RenderState { display = dpy, buffer = w } = withXftDraw dpy w vis colormap where
  (vis, colormap) = visualColormap dpy

withDrawState :: RenderState -> (XftDraw -> DrawCall ()) -> DrawCall ()
withDrawState rs action = do
  s' <- get >>= \s -> liftIO $ withDraw rs $ \d -> execStateT (action d) s
  put s'

withDrawWidget :: RenderState -> Maybe XftDraw -> WidgetAttributes -> (XftDraw -> DrawCall ()) -> DrawCall ()
withDrawWidget rs globalDraw attr action = do
  let WidgetAttributes (Size ws hs) (Size x y) _ wbg _ _ = attr
  let clipAndDraw d = do
        void $ liftIO $ do
          xftDrawSetClipRectangles d 0 0 [Rectangle (fi x) (fi y) (fi ws) (fi hs)]
          drawRect (display rs) d wbg x y ws hs
        action d

  case globalDraw of
    Just d -> clipAndDraw d
    Nothing -> withDrawState rs $ \d -> clipAndDraw d


withColor :: Display -> String -> (XftColor -> IO ()) -> IO ()
withColor dpy = withXftColorName dpy vis colormap where
  (vis, colormap) = visualColormap dpy


drawMessage rs attr tattr font d (icons, off) (Text fg bg msg) = do
  let WidgetAttributes sz pos  _ wbg _ _ = attr
  let TextAttributes wfg justify _ ths = tattr
  let dpy = display rs
  glyphInfo <- liftIO $ xftTextExtents dpy font msg
  let (Size ws _, Size x y, Size xoff yoff) = (sz, pos, off)
  let [dx, dy, twidth, txoff] = map ($ glyphInfo) [
       xglyphinfo_x, xglyphinfo_y, xglyphinfo_width, xglyphinfo_xOff]
  let x' = x + case justify of
                  JustifyLeft -> dx
                  JustifyMiddle -> (ws - txoff) `div` 2
                  JustifyRight ->  ws - txoff
  let y' = y + ((ths + dy) `div` 2)
  let fg' = fromMaybe wfg fg
  -- liftIO $ print (show msg ++ "  " ++ show x' ++ " " ++ show y' ++ " w:" ++ show txoff)
  liftIO $ forM_ bg $ \c -> drawRect dpy d c (x + xoff) (y + yoff) ws ths
  liftIO $ withColor dpy fg' $ \c -> xftDrawString d c font (x' + xoff) (y' + yoff) msg
  return (icons, Size (xoff + txoff) yoff)

drawMessage rs attr tattr font d (icons, off) (IconRef icon) = do
  let WidgetAttributes sz pos  _ wbg _ _ = attr
  let (Size ws _, Size x y, Size xoff yoff) = (sz, pos, off)
  let TextAttributes _ _ _ ths = tattr

  (icons', CachedIcon width height img) <- loadIconImage icons icon
  putImage (display rs) (buffer rs) (gc_ rs)
           img 0 0 (fi $ x + xoff) (fi $ y + yoff + ((ths - height) `div` 2)) (fi width) (fi height)
  return (icons', Size (xoff + width) yoff)

drawMessages :: RenderState -> WidgetAttributes -> TextAttributes
             -> XftFont -> XftDraw -> (IconCache, Int) -> String -> IO (IconCache, Int)
drawMessages rs attr tattr font d (icons, yoff) msg = do
  let TextAttributes _ _ _ ths = tattr
  (icons', _) <- foldM (drawMessage rs attr tattr font d) (icons, Size 0 yoff) (parseLine msg)
  return (icons', yoff + ths)

drawStringWidget :: RenderState -> Widget -> XftFont -> [String] -> Maybe XftDraw -> DrawCall ()
drawStringWidget rs@RenderState { display = dpy } wd font strings globalDraw = do
  let draw = drawMessages rs (attr_ wd) (tattr_ wd) font
  withDrawWidget rs globalDraw (attr_ wd) $ \d -> do
    icons <- get
    (icons', _) <- liftIO $ foldM (draw d) (icons, 0) strings
    put icons'

makeTextPainter :: RenderState -> Widget -> IO ([String] -> DrawCallback)
makeTextPainter rs wd = do
  fn <- makeFont rs (tattr_ wd)
  return $ drawStringWidget rs wd fn

repaint :: (RenderState, ControlChan, DrawRef) -> DrawCallback -> IO ()
repaint (rs, ch, ref) drawable = do
  tid <- myThreadId
  let drawable' winids d = do
          let w = window rs
          if window rs `elem` winids then drawable d else liftIO (killThread tid)
  writeIORef ref drawable
  -- writeChan ch $ LegacyDraw drawable'

runStatelessThread :: IO a -> IO ()
--runStatelessThread action = void $ forkIO $ forever action
runStatelessThread action = return ()

runThread :: a -> (a -> IO a) -> IO ()
--runThread a f = void $ forkIO $ iterateM_ f a
runThread a f = return ()

refDraw :: IORef DrawCallback -> DrawCallback
refDraw ref d = liftIO (readIORef ref) >>= \cb -> cb d

buildWidget :: (RenderState, ControlChan, DrawRef) -> Widget -> IO DrawCallback

buildWidget ctx@(rs,_,ref) wd@CpuTop {refreshRate = refresh} = do
  draw <- makeTextPainter rs wd
  let loadavg = foldl (\a b -> a++" "++b) "Load avg: " . take 3 . words <$> readFully "/proc/loadavg"
  procs <- pickCpuUsage
  ts <- getCurrentTime

  runThread (procs,ts, []) $ \(procs', ts', top) -> do
    avg <- loadavg
    print (avg,top)
    repaint ctx $ draw (avg:top)
    threadDelay $ round (1000000 * refresh)
    procs'' <- pickCpuUsage
    ts'' <- getCurrentTime
    let dt = getDt ts'' ts'
    top <- makeCpuDiff procs'' procs' dt
    return (procs'', ts'', top)
  return $ refDraw ref

buildWidget ctx@(rs,_,ref) wd@NetStatus {netdev_ = netdev, refreshRate = refresh} = do
  draw <- makeTextPainter rs wd
  net <- netState netdev
  deltaT <- deltaTime
  writeIORef ref $ draw ["Calculating..."]

  runThread (net, deltaT, [0,0], 0) $ \(net', deltaT', total', totaldt') -> do
      threadDelay $ round (1000000 * refresh)
      (curr, net'') <- stepAuto net' ()
      (dt, deltaT'') <- stepAuto deltaT' ()
      let total'' = zipWith (+) total' curr
          totaldt'' = totaldt' + dt
          [currIn, currOut] = map (bytes . perSec dt) curr
          [avgIn, avgOut] = map (bytes . perSec totaldt'') total''
          message = printf "In: %s/s : Out: %s/s" currIn currOut
          totalMessage = printf "Avg In: %s/s : Out: %s/s" avgIn avgOut
      repaint ctx $ draw [message, totalMessage]
      return (net'', deltaT'', total'', totaldt'')
  return $ refDraw ref

buildWidget ctx@(rs,_,ref) wd@MemStatus {refreshRate = refresh} = do
  draw <- makeTextPainter rs wd
  runStatelessThread $ do
      x' <- readKeyValueFile ((`div` 1024) . read . head . words) "/proc/meminfo" :: IO (M.Map String Int)
      let x = M.insert "Swap" ((x M.! "SwapTotal") - (x M.! "SwapFree")) x'
      let values = ["MemFree", "Cached", "Buffers", "Swap", "Dirty", "Hugetlb"]
      let mem = map (\n -> printf "%7s: %4d MB" n (x M.! n)) values
      perPidInfo <- memInfo
      repaint ctx $ draw $ map (pair (++)) $ zip mem perPidInfo
      threadDelay $ round (1000000 * refresh)
  return $ refDraw ref

buildWidget ctx _ = return $ const $ return ()

deltaTime = do
  ts <- getCurrentTime
  let m ts' = mkAutoM_ $ \_ -> do
      ts'' <- getCurrentTime
      return (getDt ts'' ts', m ts'')
  return $ m ts

netState netdev = do
  net <- readNetFile "/proc/net/dev"
  let m net' = mkAutoM_ $ \_ -> do
      net'' <- readNetFile "/proc/net/dev"
      let inout = getNetBytes $ zipWith (-) (net'' ! netdev) (net' ! netdev)
      return (inout, m net'')
  return (m net)

data GraphSample = GraphSample { sample :: ![Int] } deriving Show
data GraphData = LinearGraph ![GraphSample] | LogGraph Int ![[GraphSample]] deriving Show

makeGraph :: TimeScale -> Int -> Int -> GraphData
makeGraph LinearTime ws l = LinearGraph $ replicate ws $ GraphSample $ replicate l 0
makeGraph (LogTime n) ws l = LogGraph n $ replicate (1 + ws `div` n) []

avgSamp :: [GraphSample] -> GraphSample -- (a + b + 1) /2
avgSamp ar = GraphSample $ map (\v -> (sum v + 1) `div` length v) $ transpose $ map (\(GraphSample s) -> s) ar

updateGraph ws (LinearGraph g) s = LinearGraph $ s : take ws g
updateGraph ws (LogGraph n g) s = ($!) LogGraph n $ updateLayer g s where
  updateLayer :: [[GraphSample]] -> GraphSample -> [[GraphSample]]
  updateLayer [] _ = []
  updateLayer (vv:xs) v
    | length vv == (n+2) = let (a,b) = splitAt n vv in (v:a):updateLayer xs (avgSamp b)
    | otherwise = (v:vv):xs

exportGraph (LinearGraph g) = transpose $ map (\(GraphSample s) -> s) g
exportGraph (LogGraph n g) = transpose $ map (\(GraphSample s) -> s) $ concatMap (take' n) g where
  take' n arr = let (a,b) = splitAt (n-1) arr in a ++ (case b of
       [] -> []
       _ -> [avgSamp b])

readBatteryFile = readKeyValueFile $ head . words
readNetFile = readKeyValueFile $ map read . words
getDt newTs ts = (/1e12) . fromIntegral . fromEnum $ diffUTCTime newTs ts

getNetBytes input = [inbound, outbound] where
  atIndex idx = fi $ input !! idx
  inbound = atIndex 0
  outbound = atIndex 8

makeSegment y0 height (x,y) = Segment x' y0' x' y1' where
  x' = fi x
  y0' = fi $ height + y0
  y1' = fi $ height + y0 - fi y

drawColorSegment rs (segments, color) = do
  let RenderState {display = dpy, buffer = w, gc_ = gc} = rs
  setForeground dpy gc color
  drawSegments dpy w gc segments

toColor = fst . head . readHex . tail
drawRect dpy d bg x y w h = withColor dpy bg $ \c -> xftDrawRect d c x y w h

flp o = if o == Horizontal then Vertical else Horizontal
dir o = if o == Horizontal then Size 1 0 else Size 0 1
clamp (Size w h) = Size (max 0 w) (max 0 h)

reserve :: WidgetAttributes -> Size -> Size -> Orientation -> (WidgetAttributes, Size)
reserve wa wpos wsz ort =
  let WidgetAttributes sz _ p@(Padding plt prb) bg cmd tip = wa
      newpos = clamp $ wsz - dir ort * (sz + plt + prb)
      sz' = wsz - dir ort * newpos - plt - prb
      pos' = wpos + dir ort * newpos + plt
      in (WidgetAttributes sz' pos' p bg cmd tip, newpos)

layoutTooltip :: Maybe Tooltip -> Maybe Tooltip
layoutTooltip Nothing = Nothing
layoutTooltip (Just (Tooltip bg sz orien wds)) = Just $ Tooltip bg sz orien $ layoutWidgets orien sz wds

layoutWidget :: Size -> Orientation -> Size -> Widget -> (Size, [Widget])
layoutWidget wpos ort wsz (Frame wa cort cwds) =
  let (WidgetAttributes ws pos _ _ _ _, newpos) = reserve wa wpos wsz ort
      cwds' = concat $ snd $ mapAccumL (layoutWidget pos cort) ws cwds
   in (newpos, cwds')

layoutWidget wpos ort wsz wd =
  let (wa', newpos) = reserve (attr_ wd) wpos wsz ort
      wa'' = wa' { mbtooltip = layoutTooltip $mbtooltip wa' }
   in (newpos, [wd { attr_ = wa'' }])

layoutWidgets :: Orientation -> Size -> [Widget] -> [Widget]
layoutWidgets orien wsz =
  concat . snd . mapAccumL (layoutWidget (Size 0 0) orien) wsz


windowMapAndSelectInput :: Display -> Window -> Word64 -> IO ()
windowMapAndSelectInput dpy w mask = do
  selectInput dpy w mask
  mapWindow dpy w
  sync dpy False
  flush dpy

sendClientEvent :: Display -> Atom -> Window -> Atom -> IO ()
sendClientEvent d a w val = do
    allocaXEvent $ \e -> do
         setEventType e clientMessage
         setClientMessageEvent e w a 32 val currentTime
         sendEvent d w False structureNotifyMask e
    sync d False

copyChanToX :: ControlChan -> Window -> IO ()
copyChanToX chan w = do
  ch <- dupChan chan
  d <- openDisplay ""
  a <- internAtom d "BAR_UPDATE" False
  forever $ do
     val <- readChan ch
     sendClientEvent d a w 0 `catchIOError`  \x -> do
       print $ "Exception caught: " ++ show x
       sync d False

makeBar :: Display -> ControlChan -> Bar -> IO WindowState
makeBar dpy controlCh (Bar bg height scr gravity wds) = do
  let width = fi $ displayWidth dpy scr
  let scrHeight = displayHeight dpy scr

  -- left, right, top, bottom,
  -- left_start_y, left_end_y, right_start_y, right_end_y,
  -- top_start_x, top_end_x, bottom_start_x, bottom_end_x
  let (y, strutValues) = if gravity == GravityTop
      then (0, [0, 0, fi height, 0,
                     0, 0, 0, 0,
                     0, fi width, 0, 0])
      else (fi scrHeight - height, [0, 0, 0, fi height,
                     0, 0, 0, 0,
                     0, 0, 0, fi width ])

  w <- createWindow dpy (rootWindowOfScreen (screenOfDisplay dpy scr))
                    0 (fi y) (fi width) (fi height)
                    0 copyFromParent inputOutput (defaultVisual dpy scr) 0 nullPtr
  gc <- createGC dpy w
  buf <- createPixmap dpy w (fi width) (fi height) (defaultDepth dpy scr)

  let rs = RenderState { display = dpy, window = w, buffer = buf, gc_ = gc,
                         windowWidth = width, windowHeight = height,
                         windowBackground = bg,
                         screenNum = scr}

  strutPartial <- internAtom dpy "_NET_WM_STRUT_PARTIAL" False
  changeProperty32 dpy w strutPartial cARDINAL propModeReplace strutValues
  strut <- internAtom dpy "_NET_WM_STRUT" False
  changeProperty32 dpy w strut cARDINAL propModeReplace (take 4 strutValues)

  dockAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi dockAtom]

  windowMapAndSelectInput dpy w $ exposureMask
                              .|. structureNotifyMask
                              .|. buttonPressMask
                              .|. enterWindowMask
                              .|. leaveWindowMask
                              .|. pointerMotionMask

  let widgets = layoutWidgets Horizontal (Size width height) wds
  return $ WindowState rs widgets

------ FIXME: remove -------
getRootTimerChangeEvents :: RootInput -> Maybe TimerCollectionInp
getRootTimerChangeEvents (RTitle msg) = get' $ words msg where
  sec = fromRational 1 :: NominalDiffTime
  makeDt dt = fromIntegral (round $ dt * 1000000) * sec / 1000000
  get' ("c":n:_) = Just $ TCChangeUsage [(makeDt $ read n, 1)]
  get' ("d":n:_) = Just $ TCChangeUsage [(makeDt $ read n, -1)]
  get' _         = Nothing
getRootTimerChangeEvents _ = Nothing
--------- cut here----------

getMotionEvent :: RootInput -> Maybe (Window, Maybe Size)
getMotionEvent (RMotion w pos) = Just (w, pos)
getMotionEvent _ = Nothing

getClickEvent :: RootInput -> Maybe (Window, Size)
getClickEvent (RClick w pos) = Just (w, pos)
getClickEvent _ = Nothing

graphScale = 10000
readCPU = map read . tail. words . head . lines <$> readFully "/proc/stat"

step a = do
  (_, a') <- stepAuto a (1, epoch)
  step a'

main2 = do
  let g = createGraph (GraphDef Cpu LinearTime 1, 200)
  step g

run v = do
  v' <- ($!) return $ updateGraph 200 v (GraphSample [1,2,3,4])
  ($!) run v'

main3 :: IO ()
main3 = do
  let v = LinearGraph $ replicate 200 $ GraphSample [1,2,3,4]
  ($!) run v

createGraph :: (GraphDef,Int) -> Auto IO (Period, UTCTime) (Maybe (GraphDef, GraphData))
createGraph (def@(GraphDef Cpu tscale period), ws) = proc t -> do
    matchPeriod <- emitOn (period ==) -< fst t
    sample <- perBlip getSample -< matchPeriod
    sample2 <- dropB 1 -< sample -- drop initial bad sample
    graph <- perBlip getGraph -< fmap GraphSample sample2
    id <<< asMaybes -< fmap (\x -> (def, x)) graph
  where
    layers Cpu = 3
    layers (Net _) = 3
    layers Mem = 2
    layers Battery = 1

    scale (total:vals) = map ((`div` (if total == 0 then 1 else total)) . (*graphScale)) vals

    f n o =
      let (user:nice:sys:idle:io:_) = ($!) zipWith (-) n o
       in ([sys + io, nice, user, idle], n)

    getSample = proc t -> do
      sample <- mkState f [0,0,0] <<< effect readCPU -< t
      id -< scale . reverse . tail . scanl (+) 0 $ sample

    getGraph = proc samp -> do
      id <<< seqer <<< accum_ (updateGraph ws) (makeGraph tscale ws (layers Cpu)) -< samp

createGraph _ = mkConst Nothing

  {-
createGraph defs@(GraphDef t _ _,_) = create defs (init defs) (sampler t) where
  init (GraphDef typ tscale _, ws)= makeGraph tscale ws (layers typ)
  layers Cpu = 3
  layers (Net _) = 3
  layers Mem = 2
  layers Battery = 1
  sampler Cpu = ($!) mkAutoM_ $ \_ -> do
    cpu <- readCPU
    let sampleCpu cpu' = ($!) mkAutoM_ $ \_ -> do
        cpu'' <- readCPU
        let (user:nice:sys:idle:io:_) = ($!) zipWith (-) cpu'' cpu'
        ($!) return ([sys + io, nice, user, idle], ($!) sampleCpu cpu'')
    ($!) return ([0,0,0,1], ($!) sampleCpu cpu)
  sampler Mem = ($!) mkAutoM_ $ \_ -> do
    mem <- ($!) readBatteryFile "/proc/meminfo"
    let [total,free,cached] = ($!) map (read . (mem !))
           ["MemTotal", "MemFree", "Cached"]
    ($!) return ([total - free - cached, total - free, total], ($!) sampler Mem)
  sampler (Net netdev) = ($!) mkAutoM_ $ \_ -> do
    let f x = log (x + 1)
        f2 x = f x * f x * f x
    net <- ($!) fmap (map f2) <$> netState netdev
    maxspeed <- ($!) fmap (f2 . (*100000000)) <$> deltaTime
    let m net' maxspeed' = ($!) mkAutoM_ $ \_ -> do
        ([inbound, outbound], net'') <- ($!) stepAuto (seqer <<< net') ()
        (maxs, maxspeed'') <- ($!) stepAuto (seqer <<< maxspeed') ()
        let out = map (truncate . max 0)
                  [inbound, maxs - inbound - outbound, outbound, 0]
        ($!) return (out, ($!) m net'' maxspeed'')
    ($!) return ([0,0,0], ($!) m net maxspeed)
  sampler _ = ($!) mkAutoM_ $ \_ -> ($!) return ([0,0], ($!) sampler Mem)
  -}

dump :: (Show a) => String -> Auto IO a ()
dump msg = mkAutoM_ $ \inp -> do
  print (msg, inp)
  return ((), dump msg)

mergeRect (Rectangle x0 y0 w0 h0) (Rectangle x1 y1 w1 h1) = Rectangle x y (fi w) (fi h)
  where
  calc a0 a1 b0 b1 = let (mn, mx) = (min a0 a1, max b0 b1) in (mn, mx - mn)
  (x, w) = calc x0 x1 (x0+fi w0) (x1+fi w1)
  (y, h) = calc y0 y1 (y0+fi h0) (y1+fi h1)

combinePaints paints = M.elems $ foldr f M.empty paints where
  f (rs, rect) = M.insertWith merge (window rs) (rs, rect)
  merge (rs, rect1) (_, rect2) = (rs, mergeRect rect1 rect2)

data ZEvent = ZNop | REv !RootInput | TEv !(Period, UTCTime)
                   | GEv !(GraphDef, GraphData) deriving Show

createTooltip :: Display -> ControlChan ->  (Window, ScreenNumber)
              -> Widget -> Tooltip
              -> IO (WindowDraw, Window)
createTooltip dpy ch (parent_w, parent_scr) pwd tip = do
  let Tooltip bg tsz@(Size width height) orien widgets = tip

  parent <- getWindowAttributes dpy parent_w
  let WidgetAttributes sz pos _ _ _ _ = attr_ pwd
  let wpos = Size (fi $ wa_x parent) (fi $ wa_y parent)
  let place = if y_ wpos == 0
                 then wpos + pos + sz * dir Vertical - half (tsz-sz) * dir Horizontal - Size 0 0
                 else wpos + pos - half (tsz-sz) * dir Horizontal - tsz * dir Vertical + Size 0 0
  let screenWidth = fi $ displayWidth dpy parent_scr
  let x = max 0 $ min (x_ place) (screenWidth - width)

  print $ "Enter! Creating Window " ++ show place ++ " wpos " ++ show wpos ++ " size " ++ show tsz

  let visual = defaultVisual dpy parent_scr
      attrmask = cWOverrideRedirect
  w <- allocaSetWindowAttributes $ \attributes -> do
         set_override_redirect attributes True
         createWindow dpy (rootWindowOfScreen (screenOfDisplay dpy parent_scr))
                    (fi x) (fi $ y_ place)
                    (fi width) (fi height) 0 copyFromParent
                    inputOutput visual attrmask attributes

  tooltopAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_TOOLTIP" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi tooltopAtom]

  gc <- createGC dpy w
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  buf <- createPixmap dpy w (fi width) (fi height) (defaultDepth dpy parent_scr)

  windowMapAndSelectInput dpy w (structureNotifyMask .|. exposureMask)

  let rs = RenderState dpy w buf gc width height bg parent_scr
  let autos = bgWidget rs : map (makeWidget (rs,ch)) widgets
  let res = proc x -> do
      dump "Tip evt" -< x
      id <<< zipAuto ZNop autos -< repeat x
  return (res, w)

updateTooltip :: Display -> ControlChan -> Maybe Window
              -> Auto IO (Maybe ((Window, ScreenNumber), Widget))
                         (Maybe WindowDraw)
updateTooltip dpy ch oldw = mkAutoM_ $ \inp -> do
  forM_ oldw (\w -> print "destroy" >> destroyWindow dpy w)
  case inp of
    Nothing       -> return (Nothing, updateTooltip dpy ch Nothing)
    Just (winScr, wd) ->
      case mbtooltip $ attr_ wd of
        Nothing -> return (Nothing, updateTooltip dpy ch Nothing)
        Just tip -> do
          (tip, neww) <- createTooltip dpy ch winScr wd tip
          return (Just tip, updateTooltip dpy ch $ Just neww)

tooltipContainer :: Maybe WindowDraw
                 -> Auto IO (Blip (Maybe WindowDraw), [ZEvent], [ZEvent]) [DrawResult]
tooltipContainer mbtip = mkAutoM_ $ \(newtip, initevts, evts) -> do
  (initres, mbtip') <- case newtip of
         Blip (Just t) -> step t initevts []
         Blip Nothing -> return ([], Nothing)
         _ -> return ([], mbtip)
  case mbtip' of
      Nothing -> return ([], tooltipContainer Nothing)
      Just tip -> do
        (res, mbtip'') <- step tip evts []
        return (initres ++ res, tooltipContainer mbtip'')
  where
    step :: WindowDraw -> [ZEvent] -> [[DrawResult]]
         -> IO ([DrawResult], Maybe WindowDraw)
    step tip [] out = return (concat out, Just tip)
    step tip (e:xs) out = do
      (v, tip') <- ($!) stepAuto (seqer <<< tip) e
      step tip' xs (v : out)

dataTask :: Display -> RootChan -> ((Window, Maybe Size) -> Maybe ((Window, ScreenNumber), Widget))
                    -> [Auto IO (Period, UTCTime) (Maybe (GraphDef, GraphData))]
                    -> [WidgetDraw]
                    -> Auto IO RootInput ()
dataTask dpy ch mouse graphs widgets = proc evt -> do
    -- dump "Event === " -< evt

    tooltipChangeReq <- onChange_ <<< holdWith_ Nothing
                    <<< modifyBlips mouse
                    <<< emitJusts getMotionEvent -< evt
    -- dump "Tooltip === " -< tooltipChangeReq
    tooltipChange <- perBlip (updateTooltip dpy ch Nothing) -< tooltipChangeReq

    changeTimerBlip <- emitJusts getRootTimerChangeEvents -< evt -- FIXME: remove timer debug
    timerBlip <- onJusts <<< arrM getRootTickEvents -< evt

    let timerCollectBlips = mergeL changeTimerBlip timerBlip
    timerCollectionEvts <- fromBlips TCNop -< timerCollectBlips
    timerEvts <- timerTask ch -< timerCollectionEvts
    -- accelOverList $ dump "Timers === " -< timerEvts
    dataBlips <- accelOverList $ zipAuto dummy graphs -< map repeat timerEvts
    let graphChanges :: [(GraphDef, GraphData)]
        graphChanges = catMaybes . concat $ dataBlips
    latestGraphs <- map GEv . M.toList <$> accum_ (foldr (\(k,v) m -> M.insert k v m)) M.empty -< graphChanges
    -- accelOverList $ dump "Graphs changes === " -< graphChanges
    let zevts = (REv evt : map GEv graphChanges) ++ map TEv timerEvts
    -- accelOverList $ dump "Paint input === " -< zevts
    paints <- accelOverList $ zipAuto ZNop widgets -< map repeat zevts
    tipPaints <- tooltipContainer Nothing -< (tooltipChange, latestGraphs, zevts)

    let rect = combinePaints . catMaybes $ concat paints ++ tipPaints
    -- accelOverList $ dump "Rectangles === " -< rect
    accelOverList (mkFuncM updateWindow) -< rect
    -- id -< concat dataBlips
    id -< ()
  where
    dummy = (1, epoch)
    updateGraphSet st inp = S.union st $ S.fromList inp
    updateWindow (RenderState {display = dpy, buffer = buf, window = w, gc_ = gc},
                  Rectangle x y width height) = do
      copyArea dpy buf w gc (fi x) (fi y) (fi width) (fi height) (fi x) (fi y)
      sync dpy False

getBounds wd =
  let WidgetAttributes sz pos  _ _ _ _ = attr_ wd
  -- FIXME: use Word32 for dimensions?
  in Rectangle (fi $ x_ pos) (fi $ y_ pos) (fi $ x_ sz) (fi $ y_ sz)

myCache f = mkAutoM_ $ \_ -> do
  v <- f
  return (v, mkConst v)

mkDrawStringWidget :: RenderState -> Widget -> Auto IO [String] (RenderState, Rectangle)
mkDrawStringWidget rs@RenderState { display = dpy } wd = proc strings -> do
  -- FIXME cache_ didn't work
    fn <- myCache (makeFont rs $ tattr_ wd) -< ()
    draw -< (fn, strings)
  where
    WidgetAttributes (Size ws hs) (Size x y) _ wbg _ _ = attr_ wd
    draw :: Auto IO (XftFont, [String]) (RenderState, Rectangle)
    draw = mkAutoM_ $ \inp -> makeIconCache dpy >>= \icons -> draw' icons inp
    draw' icons (fn, strings) = do
      print ("Draw Strings: ", strings)
      (icons', _) <- withDraw rs $ \d -> do
        xftDrawSetClipRectangles d 0 0 [Rectangle (fi x) (fi y) (fi ws) (fi hs)]
        drawRect (display rs) d wbg x y ws hs
        foldM (drawMessages rs (attr_ wd) (tattr_ wd) fn d) (icons, 0) strings
      return ((rs, getBounds wd), mkAutoM_ $ draw' icons')

data Pin a = InOut a | Out deriving Show

lastIn v (InOut s) = s
lastIn v Out = v

wrapAction :: a
           -> (ZEvent -> Maybe (Pin a))
           -> Auto IO a (RenderState, Rectangle)
           -> WidgetDraw
wrapAction dflt narrow action = proc ev -> do
  v <- accumB_ lastIn dflt <<< mapMaybeB narrow -< Blip ev
  id <<< asMaybes <<< perBlip action -< v

type DrawResult = Maybe (RenderState, Rectangle)
type WidgetDraw = Auto IO ZEvent DrawResult
type WindowDraw = Auto IO ZEvent [DrawResult]

bgWidget rs = wrapAction () narrow paint' where
  narrow (REv (RExpose w)) | w == window rs  = Just Out
  narrow _     = Nothing 
  paint' = effect $ do
    let RenderState dpy ww b gc w h bg _ = rs
    withDraw rs $ \d -> drawRect dpy d bg 0 0 w h
    return (rs, Rectangle 0 0 (fi w) (fi h))

makeWidget :: (RenderState, ControlChan) -> Widget -> WidgetDraw
makeWidget (rs,_) wd@Title {} = wrapAction [""] narrow (mkDrawStringWidget rs wd)where
    narrow (REv (RTitle s)) = Just $ InOut [s]
    narrow (REv (RExpose w)) | w == window rs  = Just Out
    narrow _     = Nothing 

makeWidget (rs,_) wd@Clock { refreshRate = rate } = wrapAction epoch narrow make where
  narrow (TEv (ival, t)) | ival == rate = Just $ InOut t
  narrow (REv (RExpose w)) | w == window rs  = Just Out
  narrow _     = Nothing 
  make = proc t -> do
    tz <- myCache (case tz_ wd of
          LocalTimeZone -> localTimezone
          OtherTimeZone z -> otherTimezone z) -< ()
    -- FIXME: ignores s
    s <- mkFuncM (formatClock (fmt_ wd)) -< tz
    res <- mkDrawStringWidget rs wd -< [s]
    id -< res

makeWidget (rs,_) wd@Label { label_ = msg } = wrapAction [msg] narrow (mkDrawStringWidget rs wd) where
    narrow (REv (RExpose w)) | w == window rs  = Just Out
    narrow _     = Nothing 
   
makeWidget (rs,_) wd@(Graph attr def colors) = wrapAction (LinearGraph []) narrow painter where
    narrow (GEv (def', grdata)) | def == def'  = Just $ InOut grdata
    narrow (REv (RExpose w))  | w == window rs  = Just Out
    narrow _     = Nothing 
    painter = mkAutoM_ $ \grdata -> do
      let WidgetAttributes sz pos  _ bg _ _ = attr
          (Size ws hs, Size x0 y0) = (sz, pos)
      let scale = graphScale `div` hs
      let samp = map (map (`div` scale)) $ exportGraph grdata
      let colorTable = map toColor colors
      -- print ("Paint Graph", def, samp)
      withDraw rs $ \d -> do
        drawRect (display rs) d bg x0 y0 ws hs
        let segments = map (map (makeSegment y0 hs) . filter ((/=0) . snd)
                            . zip [x0+ws-1,x0+ws-2..]) samp
        -- print ("Paint Graph", segments, samp)
        mapM_ (drawColorSegment rs) $ zip segments colorTable
      -- FIXME: update only changed part
      return ((rs, getBounds wd), painter)

makeWidget ctx wd = proc ev -> do
  dump "makeWidget" -< ev
  id -< Nothing

extractGraphs (Graph attr graph _) = [(graph, x_ $ size attr)]
extractGraphs _ = []

extractAllGraphs wd = extractGraphs wd ++ fromTooltip where
  fromTooltip = case mbtooltip $ attr_ wd of
      Nothing -> []
      Just (Tooltip _ _ _ wds) -> concatMap extractGraphs wds

inbounds :: WidgetAttributes -> Size -> Bool
inbounds WidgetAttributes {size = (Size ws hs), position = (Size wx wy)} (Size x y) =
  x > wx && x < wx + ws && y > wy && y < wy + hs

mouseHitWds :: [Widget] -> Size -> Maybe Widget
mouseHitWds wds pos =
  let match wd = inbounds (attr_ wd) pos 
   in find match wds

mouseHitWins :: [WindowState] -> (Window, Maybe Size) -> Maybe ((Window, ScreenNumber), Widget)
mouseHitWins wins (w, Nothing) = Nothing
mouseHitWins wins (w, Just pos) =
  let matchWin (WindowState rs _) = w == window rs
      matchWds (WindowState rs wds) = mouseHitWds wds pos >>= \wd -> Just ((window rs, screenNum rs), wd)
   in find matchWin wins >>= matchWds

initRootTask :: Display -> ControlChan -> [WindowState] -> IO(Auto IO RootInput ())
initRootTask dpy ch wins = do
  forkIO $ forever $ do
    line <- getLine
    writeChan ch (RTitle line)
  let graphs = map createGraph $ concatMap extractAllGraphs $ concatMap getWidgets wins
      mouse = mouseHitWins wins
      widgets = concatMap mkWindow wins
  return $ dataTask dpy ch mouse graphs widgets where
        getWidgets (WindowState _ wds) = wds
        getWidgetAndRs (WindowState rs wds) = zip wds (repeat rs)
        mkWindow (WindowState rs wds) = bgWidget rs : map (makeWidget (rs,ch)) wds


eventLoop dpy ch auto = do
  rootEv <- ($!) allocaXEvent $ \ev -> do
    nextEvent dpy ev
    event <- getEvent ev
    case event of
       ClientMessageEvent {ev_data = 0:_} -> liftIO $ readChan ch
       ExposeEvent { ev_window = w } ->      return $ RExpose w
       MotionEvent {ev_x = x, ev_y = y, ev_window = ww} ->
         return $ RMotion ww $ Just (Size (fi x) (fi y))
       
       ev@CrossingEvent {ev_x = x, ev_y = y, ev_window = ww} ->
         if ev_event_type ev == enterNotify
            then return $ RMotion ww $ Just (Size (fi x) (fi y))
            else return $ RMotion ww Nothing
       _ -> return RNop
  (_, auto') <- stepAuto auto rootEv
  eventLoop dpy ch auto'

main :: IO ()
main = do
  --main3
  -- main2
  xSetErrorHandler
  dpy <- openDisplay ""
  -- FIXME: increase to improve throughput?
  controlCh <- newChan

  wins <- mapM (makeBar dpy controlCh) bars

  let WindowState firstRs _ = head wins
  forkOS $ copyChanToX controlCh $ window firstRs

  auto <- initRootTask dpy controlCh wins
  (_, auto1) <- stepAuto auto $ RTitle "c 0.05"
  (_, auto2) <- stepAuto auto1 $ RTitle "c 0.1"
  (_, auto3) <- stepAuto auto2 $ RTitle "c 1"
  eventLoop dpy controlCh auto3
  return ()

