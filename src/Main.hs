{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Loops
import Data.Bits
import Data.IORef
import Data.List
import Data.Map ((!))
import Data.Maybe
import Data.Time
import Data.Time.Zones
import Data.Word
import Foreign.Ptr
import GHC.Generics (Generic)
import Graphics.X11.Xft
import Graphics.X11.Xinerama
import Graphics.X11.Xlib hiding (Screen)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xrender
import Graphics.X11.Xrandr
import Numeric
import System.Exit
import System.IO.Error
import System.Posix.Process
import System.Process (runCommand, terminateProcess)
import Text.Printf
import qualified Control.Category as Cat
import qualified GHC.Exts.Heap as Heap
import qualified GHC.Exts.Heap.Closures as Closures

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Prelude hiding (id)

import DzenParse
import Icon
import Timer
import Top
import Utils
import Text.Parsec.Token (GenTokenParser(dot))

barHeight :: Int
barHeight = 24

defaultFont :: String
defaultFont = "-*-*-medium-r-normal--15-*-*-*-*-*-iso10646-*"

largeFont = "-*-*-medium-r-normal--35-*-*-*-*-*-iso10646-*"

barBackground :: String
barBackground = "#BEBEBE"

infoBackground :: String
infoBackground = "#181838"

tooltipBackground :: String
tooltipBackground = "#FFFFC0"

defaultTextColor :: String
defaultTextColor = "#C7AE86"

trayerCmd :: Int -> Int -> String
trayerCmd = printf "trayer --expand false --edge top --align right\
             \ --widthtype request --height %d --margin %d"

--bars :: [Bar]
bars = [bar1, bar2]
--bars = [bar1]

bar1 :: Bar
bar1 = Bar barBackground barHeight (XineramaScreen 0) GravityTop [
        clock # TimeFormat "%R" # RefreshRate 60 # OnClick "clock.sh"
              # Width 60 # RightPadding 4
              # LocalTimeZone # BackgroundColor infoBackground
              # clockTooltip,
        cpuBars,
        logtm cpu # cpuTooltip # OnClick "top.sh",
        logtm mem # memTooltip,
        logtm (net "eth0"),
        logtm (net "brkvm"),
        logtm (net "wlp2s0"),
        logtm (net "wlp0s20f3"),
        battery "BAT0",
        battery "BAT1",
        trayer,

        title # LeftPadding 5 # RightPadding 2 #
                BackgroundColor barBackground #
                JustifyLeft # TextColor "#000000"
      ]

bar2 :: Bar
bar2 = Bar barBackground barHeight (XineramaScreen 1) GravityTop [
        clock # TimeFormat "%R" #RefreshRate 60 #
            Width 60 # RightPadding 4 #
            LocalTimeZone # BackgroundColor infoBackground #
            clockTooltip,
        cpuBars,
        logtm cpu # cpuTooltip # OnClick "top.sh" #LinearTime # RefreshRate 1,

        title # LeftPadding 2 # RightPadding 2 #
                BackgroundColor barBackground #
                JustifyLeft # TextColor "#000000"
      ]

clockTooltip :: Tooltip
clockTooltip = Tooltip tooltipBackground (Size 560 (barHeight * 5)) Vertical [
     tooltipClock #TimeFormat "[%U] %a, %e %b - %X" #JustifyMiddle
              # Height (barHeight * 2) # RightPadding 4 # LocalTimeZone
              #SetFont largeFont #SetTextHeight (2 *barHeight),
     frame Horizontal [
          frame Vertical [
                 tooltipClock #OtherTimeZone "America/Los_Angeles"
                               -- 0527 06:17:59.956236
                              #TimeFormat "%Y-%m-%d %H:%M:%S",
                 tooltipClock #OtherTimeZone "America/Los_Angeles",
                 tooltipClock #OtherTimeZone "GMT"
                 ] #Width 340,
          frame Vertical [
                 tooltipLabel #Message "MTV TS: " #JustifyRight,
                 tooltipLabel #Message "MTV: " #JustifyRight,
                 tooltipLabel #Message "GMT: " #JustifyRight
                 ]
          ] #Height (barHeight * 3)
     ]

cpuTooltip :: Tooltip
cpuTooltip = Tooltip tooltipBackground (Size 600 (8*barHeight)) Horizontal [
     tooltip cpu #RefreshRate 0.05 #LinearTime #Width 100
                   #BottomPadding 1 #RightPadding 1 # LeftPadding 1
                   #Width 100 #Refresh WhenVisible,
     tooltip cpu #RefreshRate 1 #LinearTime #Width 200
                   #BottomPadding 1 #RightPadding 1 # LeftPadding 1,
     tooltipText cpuTop #Width 300
     ]

memTooltip :: Tooltip
memTooltip = Tooltip tooltipBackground (Size 490 (6*barHeight)) Horizontal [
     tooltipText memstatus #Width 430 #LeftPadding 5,
     tooltip mem #RefreshRate 1 #LogTime 3
     ]

netTooltip :: String -> Tooltip
netTooltip netdev = Tooltip tooltipBackground (Size 480 (10*barHeight)) Vertical [
     frame Horizontal [
           tooltip (tooltipNet netdev) #RefreshRate 0.05 #Refresh WhenVisible #LinearTime # Width 200,
           tooltip (tooltipNet netdev) #RefreshRate 1 #LinearTime # Width 200
                           # TopPadding 1 # BottomPadding 1
           ] #Height (8 * barHeight),
     tooltipText (netstatus netdev) #RefreshRate 3 #JustifyLeft #LeftPadding 10
     ]

batteryGraphTimer :: Period
batteryGraphTimer = 75

batteryTooltip :: String -> Tooltip
batteryTooltip name = Tooltip tooltipBackground (Size 380 (7+barHeight*8)) Vertical [
     tooltip (batteryGraph name) #RefreshRate batteryGraphTimer #Height (barHeight*7)
             # BottomPadding 2  # LeftPadding 2 #RightPadding 2,
     hseparator,
     tooltipText (batteryRate name) # BottomPadding 3
     ]

logtm :: Widget -> Widget
logtm w = w # LogTime 8 # Width 129 # RefreshRate 1 -- One week worth of data

tooltip :: Widget -> Widget
tooltip w = w #BackgroundColor "#FFFFC0"
              #TopPadding 0 #BottomPadding 1 #LeftPadding 0 #RightPadding 1

tooltipText :: Widget -> Widget
tooltipText w = tooltip w  #TextColor "#000000"
              #SetFont "-*-courier new-*-r-normal-*-17-*-*-*-*-*-*-*"

tooltipClock :: Widget
tooltipClock = tooltipText clock #TimeFormat "%a, %e %b %Y - %X" #JustifyLeft

tooltipLabel :: Widget
tooltipLabel = tooltipText label

tooltipNet :: String -> Widget
tooltipNet netdev = Graph defaultAttr (GraphDef (Net netdev) (LogTime 8) Always)
                    ["#6060FF", tooltipBackground, "#60FF60"] 1  # Width 129

data Attribute = Width Int | Height Int | LeftPadding Int | RightPadding Int
               | TopPadding Int | BottomPadding Int
               | TextColor Main.Color | BackgroundColor Main.Color
               | TimeFormat String | Message String | SetFont String | SetTextHeight Int
               | RefreshRate Period | OnClick String | Refresh RefreshType

type Color = String
type Font = String
type Pos = Size

type OnClickCmd = String

data Gravity = GravityTop | GravityBottom deriving (Show, Eq)
data Screen = DefaultScreen | XineramaScreen Int deriving (Show, Eq)
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
data GraphType = Cpu | Net String | Mem | Battery String deriving (Show, Eq, Ord, Generic, NFData)
data TimeScale = LinearTime | LogTime Int deriving (Show, Eq, Ord, Generic, NFData)
data RefreshType = Always | WhenVisible deriving (Show, Eq, Ord, Generic, NFData)
data GraphDef = GraphDef {type_ :: GraphType, tscale_ :: TimeScale, refresh_type_ :: RefreshType} deriving (Show, Eq, Ord, Generic, NFData)

data Bar = Bar String Int Screen Gravity [Widget] deriving Show
data Tooltip = Tooltip String Size Orientation [Widget] deriving (Show, Eq)

data Widget = Clock   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, fmt_ :: String, tz_ :: ClockTimeZone, refreshRate_ :: Period }
          | Label   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, label_ ::  String }
          | Title   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes }
          | CpuTop  {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate_ :: Period}
          | NetStatus{attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate_ :: Period, netdev_ :: String}
          | MemStatus{attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate_ :: Period}
          | Frame   {attr_ :: WidgetAttributes, frameOrientation :: Orientation, children :: [Widget]}
          | Graph   {attr_ :: WidgetAttributes, graph_ :: GraphDef, graphColorTable :: [String], refreshRate_ :: Period}
          | BatteryStatus   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, batteryName_ :: String, refreshRate_ :: Period }
          | BatteryRate     {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, batteryName_ :: String, refreshRate_ :: Period }
          | CpuBars   {attr_ :: WidgetAttributes, graphColorTable :: [String], refreshRate_ :: Period}
          | Trayer  {attr_ :: WidgetAttributes}
          deriving (Show, Eq)

defaultAttr :: WidgetAttributes
defaultAttr = WidgetAttributes (Size 120 barHeight) 0 (Padding 1 1) infoBackground Nothing Nothing

defaultTAttr :: TextAttributes
defaultTAttr = TextAttributes defaultTextColor JustifyMiddle defaultFont barHeight

clock :: Widget
clock = Clock defaultAttr defaultTAttr "%R" LocalTimeZone 1

cpu :: Widget
cpu = Graph defaultAttr (GraphDef Cpu (LogTime 8) Always) ["#70FF70", "#FF8080", "#F020F0", "#3030FF"] 1 -- # Width 129

cpuBars = CpuBars defaultAttr ["#70FF70", "#FF8080", "#F020F0", "#3030FF"] 0.5

mem :: Widget
mem = Graph defaultAttr (GraphDef Mem (LogTime 8) Always) ["#00FF00", "#6060FF"] 1 -- # Width 129

batteryGraph :: String -> Widget
batteryGraph name = Graph defaultAttr (GraphDef (Battery name) LinearTime Always) ["#0760F2"] 1

net :: String -> Widget
net netdev = Graph defaultAttr (GraphDef (Net netdev) (LogTime 8) Always)
             ["#6060FF", infoBackground, "#60FF60"] 1 # Width 129 #netTooltip netdev

netstatusRefresh :: NominalDiffTime
netstatusRefresh = 3

netstatus :: String -> Widget
netstatus = NetStatus defaultAttr defaultTAttr netstatusRefresh

memstatus :: Widget
memstatus = MemStatus defaultAttr defaultTAttr 1 #JustifyLeft

label :: Widget
label = Label defaultAttr defaultTAttr ""

hseparator = Label (WidgetAttributes (Size 1000 1) 0 (Padding 1 1) "#c0c0c0" Nothing Nothing)
                   defaultTAttr "" # LeftPadding 20 # RightPadding 20

title :: Widget
title = Title defaultAttr defaultTAttr

cpuTop :: Widget
cpuTop = CpuTop defaultAttr defaultTAttr 3 # JustifyLeft

trayer :: Widget
trayer = Trayer defaultAttr # Width barHeight

battery :: String -> Widget
battery name = BatteryStatus defaultAttr defaultTAttr name 10 # Width 120 # batteryTooltip name

batteryRate :: String -> Widget
batteryRate name = BatteryRate defaultAttr defaultTAttr name 1

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

withAttr :: Widget -> (WidgetAttributes -> WidgetAttributes) -> Widget
withAttr ww f = ww { attr_ = f (attr_ ww) }

withPadding :: Widget -> (Padding -> Padding) -> Widget
withPadding ww f = withAttr ww $ \wa -> wa { padding = f (padding wa) }

instance Apply Attribute where
  apply (TextColor c) ww = let TextAttributes _ j f hs = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}
  apply (SetFont f) ww = let TextAttributes c j _ hs = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}
  apply (SetTextHeight hs) ww = let TextAttributes c j f _ = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}

  apply (Width w) ww = withAttr ww $ \wa -> wa { size = Size w (y_ . size $ wa)}
  apply (Height h) ww = withAttr ww $ \wa -> wa { size = Size (x_ . size $ wa) h}
  apply (OnClick cmd) ww = withAttr ww $ \wa -> wa { onclick = Just cmd }

  apply (LeftPadding l) ww = withPadding ww $ \p -> let Padding (Size _ t) pbr = p in Padding (Size l t) pbr
  apply (TopPadding t) ww = withPadding ww $ \p -> let Padding (Size l _) pbr = p in Padding (Size l t) pbr
  apply (RightPadding r) ww = withPadding ww $ \p -> let Padding plt (Size _ b) = p in Padding plt (Size r b)
  apply (BottomPadding b) ww = withPadding ww $ \p -> let Padding plt (Size r _) = p in Padding plt (Size r b)
  apply (BackgroundColor c) ww = withAttr ww $ \attr -> let WidgetAttributes ws x p _ cmd tip = attr in
                                                     WidgetAttributes ws x p c cmd tip
  apply (TimeFormat fmt) ww = ww { fmt_ = fmt }
  apply (Message s) ww = ww { label_ = s }
  apply (RefreshRate r) ww = ww { refreshRate_ = r }
  apply (Refresh t) ww@Graph{graph_ = g} = ww {graph_ = g { refresh_type_ = t } }

infixl 9 #
(#) :: (Apply a) => Widget -> a  -> Widget
(#) w a = apply a w

type TimeConv = UTCTime -> IO ZonedTime
localTimezone :: IO TimeConv
localTimezone = do
  timezone <- getCurrentTimeZone
  return $ \utcTime -> do
    let localTime = utcToLocalTime timezone utcTime
    return $ ZonedTime localTime timezone

otherTimezone :: String -> IO TimeConv
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

data RenderState = RenderState {
  display_ :: Display,
  window_ :: Window,
  buffer_ :: Pixmap,
  gc_ :: GC,
  windowWidth_ :: Int,
  windowHeight_ :: Int,
  windowBackground_ :: String,
  windowPos_ :: (Int, Int, Int, Int, Int, Int),
  titleRef_ :: IORef String
}

instance Eq RenderState where
  (==) a b = window_ a == window_ b

instance Show RenderState where
  show RenderState {} = "RenderState "

visualColormap :: Display -> (Visual, Colormap)
visualColormap dpy = (vis, colormap) where
  scr = defaultScreen dpy
  vis = defaultVisual dpy scr
  colormap = defaultColormap dpy scr

withDraw :: RenderState  -> (XftDraw -> IO a) -> IO a
withDraw  RenderState { display_ = dpy, buffer_ = w } = withXftDraw dpy w vis colormap where
  (vis, colormap) = visualColormap dpy

withColor :: Display -> String -> (XftColor -> IO ()) -> IO ()
withColor dpy = withXftColorName dpy vis colormap where
  (vis, colormap) = visualColormap dpy


drawMessage :: RenderState -> WidgetAttributes -> TextAttributes
               -> XftFont -> XftDraw -> (IconCache, Size)
               -> Message -> IO (IconCache, Size)
drawMessage rs attr tattr font d (icons, off) (Text fg bg msg) = do
  let WidgetAttributes sz pos  _ wbg _ _ = attr
  let TextAttributes wfg justify _ ths = tattr
  let dpy = display_ rs
  glyphInfo <- xftTextExtents dpy font msg
  let (Size ws _, Size x y, Size xoff yoff) = (sz, pos, off)
  let [dx, dy, txoff] = map ($ glyphInfo) [
       xglyphinfo_x, xglyphinfo_y, xglyphinfo_xOff]
  let x' = x + case justify of
                  JustifyLeft -> dx
                  JustifyMiddle -> (ws - txoff) `div` 2
                  JustifyRight ->  ws - txoff
  let yoff' = yoff + ((ths + dy) `div` 2)
  let fg' = fromMaybe wfg fg
  drawRect dpy d (fromMaybe wbg bg) (x + xoff) yoff ws ths
  withColor dpy fg' $ \c -> xftDrawString d c font (x' + xoff) yoff' msg
  return (icons, Size (xoff + txoff) yoff)

drawMessage rs attr tattr _ _ (icons, off) (IconRef icon) = do
  let (Size x y, Size xoff yoff) = (position attr, off)
  let TextAttributes _ _ _ ths = tattr

  (icons', CachedIcon width height img) <- loadIconImage icons icon
  putImage (display_ rs) (buffer_ rs) (gc_ rs)
           img 0 0 (fi $ x + xoff) (fi $ y + yoff + ((ths - height) `div` 2)) (fi width) (fi height)
  return (icons', Size (xoff + width) yoff)

drawMessages :: RenderState -> WidgetAttributes -> TextAttributes
             -> XftFont -> XftDraw -> (IconCache, Int) -> String -> IO (IconCache, Int)
drawMessages rs attr tattr font d (icons, yoff) msg = do
  let TextAttributes _ _ _ ths = tattr
  (icons', _) <- foldM (drawMessage rs attr tattr font d) (icons, Size 0 yoff) (parseLine msg)
  return (icons', yoff + ths)


-- Should be data for strictness
type GraphSample = [Int]
data GraphData = LinearGraph [GraphSample] | LogGraph Int [[GraphSample]] deriving (Show, Generic, NFData)

makeGraph :: TimeScale -> Int -> Int -> GraphData
makeGraph LinearTime ws l = LinearGraph $ replicate ws $ replicate l 0
makeGraph (LogTime n) ws _ = LogGraph n $ replicate (1 + ws `div` n) []

avgSamp :: [GraphSample] -> GraphSample -- (a + b + 1) /2
avgSamp ar = map (\v -> (sum v + 1) `div` length v) $ transpose ar

updateGraph :: Int -> GraphData -> GraphSample -> GraphData
updateGraph ws (LinearGraph g) s = LinearGraph $ s : take ws g
updateGraph _ (LogGraph n g) s = LogGraph n $ updateLayer g s where
  updateLayer :: [[GraphSample]] -> GraphSample -> [[GraphSample]]
  updateLayer [] _ = []
  updateLayer (vv:xs) v
    | length vv == (n+2) = let (a,b) = splitAt n vv in (v:a):updateLayer xs (avgSamp b)
    | otherwise = (v:vv):xs

exportGraph :: GraphData -> [GraphSample]
exportGraph (LinearGraph g) = g
exportGraph (LogGraph n g) = concatMap (take' n) g where
  take' n' ar = let (a,b) = splitAt (n'-1) ar in a ++ (case b of
       [] -> []
       _ -> [avgSamp b])

readBatteryFile :: FilePath -> IO (M.Map String String)
readBatteryFile = readKeyValueFile $ head . words

readNetFile :: FilePath -> IO (M.Map String [Int])
readNetFile = readKeyValueFile $ map read . words

readFileWithFallback :: String -> IO String
readFileWithFallback x = readFully x `catchIOError` \_ -> return "0"


batteryFile :: String -> String -> String
batteryFile n x = "/sys/class/power_supply/" ++ n ++ "/" ++ x

readBatteryString :: String -> String -> IO String
readBatteryString n x = head . lines <$> readFileWithFallback (batteryFile n x)

readBatteryInt :: String -> String -> IO Int
readBatteryInt n x = read <$> readBatteryString n x :: IO Int
readBatteryDouble n x = read <$> readBatteryString n x :: IO Double

getDt :: Fractional t => UTCTime -> UTCTime -> t
getDt newTs ts = (/1e12) . fromIntegral . fromEnum $ diffUTCTime newTs ts

getNetBytes :: [Int] -> GraphSample
getNetBytes input = [inbound, outbound] where
  atIndex idx = fi $ input !! idx
  inbound = atIndex 0
  outbound = atIndex 8

makeSegment y0 height (x,y) = Segment x' y0' x' y1' where
  x' = fi x
  y0' = fi $ height + y0
  y1' = fi $ height + y0 - fi y

drawColorSegment :: RenderState -> ([Segment], Pixel) -> IO ()
drawColorSegment rs (segments, color) = do
  let RenderState {display_ = dpy, buffer_ = w, gc_ = gc} = rs
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
layoutWidgets orien wsz wds =
  let (pos, ar) = mapAccumL (layoutWidget (Size 0 0) orien) wsz wds
      (first : others) = reverse $ concat ar
      change = pos * dir orien
      update wa = wa { position = position wa - change, size = size wa + change } in
  (withAttr first update : others)

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

data SenderX = SenderX Display Atom

makeSenderX = do
  d <- openDisplay ""
  a <- internAtom d "BAR_UPDATE" False
  return $ SenderX d a


sendX :: SenderX -> Window -> IO ()
sendX (SenderX dpy a) w = do
  sendClientEvent dpy a w 0 `catchIOError`  \x -> do
       print $ "Exception caught: " ++ show x
       sync dpy False


checkBattery wd n = do
  r <- tryIOError $ readFully $ batteryFile n "status"
  case r of
    Left _ -> print ("Unknown battery", n) >> return Nothing
    Right _ -> return (Just wd)

checkNetdev wd n = do
  net <- readNetFile "/proc/net/dev"
  return $ if M.member n net then Just wd else Nothing

removeBroken wd@Graph {graph_ = GraphDef {type_ = Battery n}} = checkBattery wd n
removeBroken wd@BatteryRate {batteryName_ = n} = checkBattery wd n
removeBroken wd@BatteryStatus {batteryName_ = n} = checkBattery wd n
removeBroken wd@Graph {graph_ = GraphDef {type_ = Net n}} = checkNetdev wd n
removeBroken wd@NetStatus {netdev_ = n} = checkNetdev wd n
removeBroken wd = return $ Just wd


layers Cpu = 3
layers (Net _) = 3
layers Mem = 2
layers (Battery _) = 1


scaleG hs (total:vals) = map ((`div` (if total == 0 then 1 else total)) . (*hs)) vals

readCPU :: IO GraphSample
readCPU = map read . tail. words . head . lines <$> readFully "/proc/stat"

readNet :: String -> IO (UTCTime, GraphSample)
readNet netdev = do
  ts <- getCurrentTime
  net <- readNetFile "/proc/net/dev"
  let inout = getNetBytes (net ! netdev)
  return (ts, inout)


f3 :: Double -> Double
f3 x = f x * f x * f x where
  f x = log (x + 1)

revf3 :: Double -> Double
revf3 x = exp (x ** (1/3)) - 1 + 0.5

data Sampler = SampleCpu GraphSample | SampleMem
              | SampleNet String UTCTime GraphSample
              | SampleBattery String deriving (Generic, NFData)

makeSampler Cpu = return $ SampleCpu [0,0,0,0,0]
makeSampler Mem = return SampleMem
makeSampler (Battery n) = return $ SampleBattery n

makeSampler (Net netdev) = do
  (time, net) <- readNet netdev
  return $ SampleNet netdev time net


readSample (SampleCpu prev_cpu) = do
  cpu <- readCPU
  let output = let (user:nice:sys:idle:io:_) = zipWith (-) cpu prev_cpu
               in [sys + io, nice, user, idle]
  return (output, SampleCpu cpu)

readSample SampleMem = do
  mem <- readBatteryFile "/proc/meminfo"
  let [total,free,cached] = map (read . (mem !))
         ["MemTotal", "MemFree", "Cached"]
      out = [total - free - cached, cached, free]
  return (out, SampleMem)

readSample s@(SampleBattery n) = do
    capacity <- readBatteryInt n "energy_full"
    remainingCapacity <- readBatteryInt n "energy_now"
    let out = [remainingCapacity, capacity - remainingCapacity]
    return (out, s)

readSample (SampleNet netdev old_time old_net) = do
  (time, net) <- readNet netdev
  let f x = log (x + 1)
      f3 x = f x * f x * f x
      dt = getDt time old_time :: Double
      [inbound, outbound] = map (f3 . fromIntegral) $ zipWith (-) net old_net :: [Double]
      maxspeed = f3 $ dt * 100000000 :: Double
      output = map (truncate . max 0)
           [inbound, maxspeed - inbound - outbound, outbound, 0] :: [Int]
  return (output, SampleNet netdev time net)


data Rect = Rect Int Int Int Int deriving (Generic, NFData)

mergeRect (Rect x0 y0 w0 h0) (Rect x1 y1 w1 h1) = Rect x y (fi w) (fi h)
  where
  calc a0 a1 b0 b1 = let (mn, mx) = (min a0 a1, max b0 b1) in (mn, mx - mn)
  (x, w) = calc x0 x1 (x0+fi w0) (x1+fi w1)
  (y, h) = calc y0 y1 (y0+fi h0) (y1+fi h1)


getBounds wd =
  let WidgetAttributes sz pos  _ _ _ _ = attr_ wd
  -- FIXME: use Word32 for dimensions?
  in Rect (x_ pos) (y_ pos) (x_ sz) (y_ sz)


writeIORef' ref v = atomicModifyIORef' ref (const (v, ()))


type Publisher a = (IORef a, IORef Bool)
type TextPublisher = Publisher [String]
data Updater = ClockUpdater String TimeConv TextPublisher
             | GraphUpdater GraphKey (Publisher GraphData)
             | NetStatusUpdater GraphKey TextPublisher
             | CpuTopUpdater (UTCTime, M.Map String (String, Int)) TextPublisher
             | MemStatusUpdater TextPublisher
             | BatteryStatusUpdater String TextPublisher
             | BatteryRateUpdater String TextPublisher
             | CpuBarsUpdater [[[Int]]] (Publisher GraphData) deriving (Generic, NFData)
type UpdaterDef = (Updater, Period)

publish :: Publisher a -> a -> IO ()
publish (ref, refdirty) v = do
  writeIORef' ref v
  writeIORef' refdirty True

stateless updater publisher f = do
  v <- f
  publish publisher v
  return updater

loadavg :: IO String
loadavg = foldl (\a b -> a++" "++b) "Load avg: " . take 3 . words <$> readFully "/proc/loadavg"

update _ u@(ClockUpdater fmt conv p) = stateless u p $ (:[]) <$> formatClock fmt conv

update _ u@(CpuBarsUpdater prevSamples publisher) = do
   values <- map (map read . tail) . takeWhile (isPrefixOf "cpu" . head) . map words . tail . lines <$> readFully "/proc/stat"
   let total = map sum values
   let idle = map (!! 3) values
   let busy = zipWith (-) total idle
   let toArr a b = [a, b]
   let newSample = zipWith toArr busy idle
   let samples = newSample : prevSamples
   let (prevSamples', prev) = splitAt 20 samples
   -- print $ "prevSamples'" ++ show prevSamples
   let graph = sort $ zipWith (zipWith (-)) newSample (head $ prev ++ prevSamples ++ [newSample])
   -- print $ show graph
   publish publisher $ LinearGraph $ concatMap (replicate 5) graph
   -- print $ "Updating "  ++ show (idle, busy)
   return $ CpuBarsUpdater prevSamples' publisher

update graphs u@(GraphUpdater key p) = stateless u p $ return $ snd . fromJust $ M.lookup key graphs

update graphs u@(NetStatusUpdater key p) = stateless u p $ do
  let (GraphKey _ _ rate) = key -- FIXME: may be I should check that code
  let graphdata = snd . fromJust $ M.lookup key graphs
  let fmt :: String -> [Int] -> String
      fmt hdr v =
          let [inbound, _, outbound, _] = map (fmtBytes . round . (/realToFrac rate) . revf3 . fromIntegral) v
             in printf "%s In: %s/s : Out: %s/s" hdr inbound outbound

      printNet (LinearGraph []) = ["Loading..."]
      printNet (LinearGraph samples@(sample:_)) =
         [fmt "" sample, fmt "Avg" $ avgSamp samples ]
  return $ printNet graphdata

update _ u@(CpuTopUpdater (ts, procs) p) = do
  ts' <- getCurrentTime
  cpu' <- readCPU
  procs' <- pickCpuUsage
  avg <- loadavg
  let dt = getDt ts' ts
  top <- makeCpuDiff procs' procs dt
  publish p (avg:top)
  return $ CpuTopUpdater (ts', procs') p

update _ u@(MemStatusUpdater p) = stateless u p $ do
  x <- readKeyValueFile ((`div` 1024) . read . head . words) "/proc/meminfo" :: IO (M.Map String Int)
  let x' = M.insert "Swap" ((x M.! "SwapTotal") - (x M.! "SwapFree")) x
  let values = ["MemFree", "Cached", "Buffers", "Swap", "Dirty", "Hugetlb"]
  let mem = map (\n -> printf "%7s: %5d MB" n (x' M.! n)) values
  zipWith (++) mem <$> memInfo

update _ u@(BatteryStatusUpdater name p) = stateless u p $ do
  capacity <- readBatteryInt name "energy_full"
  rate <- readBatteryInt name "power_now"
  remainingCapacity <- readBatteryInt name "energy_now"
  state <- readBatteryString name "status" :: IO String
  let (h, m) = (remainingCapacity * 60 `div` rate) `divMod` 60
      percent = remainingCapacity * 100 `div` capacity
  return $ (: []) $ case state of
    "Discharging" | rate /= 0 -> printf "%d%%(%d:%02d)" percent h m
    _ -> printf "%d%%C" percent

update _ u@(BatteryRateUpdater name p) = stateless u p $ do
  power <- readBatteryDouble name "power_now"
  volts <- readBatteryDouble name "voltage_now"
  return . (: []) $ printf "Current current: %.2f A" $ power / volts


drawStrings rs wd fn icons strings d = do
       let WidgetAttributes (Size ws hs) (Size x y) _ wbg _ _ = attr_ wd
       xftDrawSetClipRectangles d 0 0 [Rectangle (fi x) (fi y) (fi ws) (fi hs)]
       drawRect (display_ rs) d wbg x y ws hs
       (icons', _) <- foldM (drawMessages rs (attr_ wd) (tattr_ wd) fn d) (icons, y) strings
       return (icons', Just $ getBounds wd)

makeIcons rs@RenderState { display_ = dpy} = makeIconCache dpy

data TextPainter = TextPainter XftFont IconCache

makeFont :: RenderState -> TextAttributes -> IO XftFont
makeFont RenderState { display_ = dpy } (TextAttributes _ _ fontName _) =
  xftFontOpenXlfd dpy (defaultScreenOfDisplay dpy) fontName

makeTextPainter rs tattr = do
  fn <- makeFont rs tattr
  icons <- makeIcons rs
  return (TextPainter fn icons)

drawTextPainter rs wd (TextPainter fn icons) strings = do
  (icons', rect) <- withDraw rs (drawStrings rs wd fn icons strings)
  return (TextPainter fn icons', rect)

data Painter = StaticTextPainter { textp_ :: TextPainter, staticstr_ :: [String] }
             | RefTextPainter { textp_ :: TextPainter, strref_ :: IORef [String], refdirty_ :: IORef Bool }
             | RefTitlePainter { textp_ :: TextPainter, current_title_ :: String }
             | RefGraphPainter { refgraph_ :: IORef GraphData, refdirty_ :: IORef Bool }
             | NoPainter
type DynamicWidgetDef = (Painter, Maybe UpdaterDef)
data Repaint = RepaintAll | RepaintUpdated deriving (Generic, NFData)

makeUpdatingWidget rate constr ref updater  = do
  refdirty <- newIORef True
  return (constr ref refdirty, Just (updater (ref, refdirty), rate))


makeUpdatingTextWidget' :: [String]
                       -> RenderState
                       -> Widget
                       -> NominalDiffTime
                       -> ((IORef [String], IORef Bool) -> Updater)
                       -> IO DynamicWidgetDef
makeUpdatingTextWidget' initmsg rs wd rate text_updater = do
  textp <- makeTextPainter rs (tattr_ wd)
  refstr <- newIORef initmsg
  makeUpdatingWidget rate (RefTextPainter textp) refstr text_updater


makeUpdatingTextWidget = makeUpdatingTextWidget' []


drawIfUpdated rs wd p = do
  let refdirty = refdirty_ p
  is_dirty <- readIORef refdirty
  if is_dirty
     then do
       writeIORef' refdirty False
       draw rs wd RepaintAll p
     else return (p, Nothing)


draw rs wd RepaintAll p@(StaticTextPainter textp strings) = do
  (textp', rect) <- drawTextPainter rs wd textp strings
  return (p{textp_ = textp'}, rect)

draw rs wd RepaintUpdated p@RefTextPainter{} = drawIfUpdated rs wd p
draw rs wd RepaintUpdated p@RefGraphPainter{} = drawIfUpdated rs wd p

draw rs wd RepaintAll p@RefTextPainter {textp_ = textp, strref_ = strref} = do
  strings <- readIORef strref
  (textp', rect) <- drawTextPainter rs wd textp strings
  return (p{textp_ = textp'}, rect)

draw rs wd RepaintUpdated p@RefTitlePainter{current_title_ = prev} = do
  curr <- readIORef (titleRef_ rs)
  if curr /= prev then draw rs wd RepaintAll p else return (p, Nothing)

draw rs wd RepaintAll p@RefTitlePainter {textp_ = textp} = do
  title <- readIORef (titleRef_ rs)
  let empty = title == ""
  --print $ "Title is empty: " ++ show empty
  (if empty then unmapWindow else mapWindow) (display_ rs) (window_ rs)
  (textp', rect) <- drawTextPainter rs wd textp [ title ]
  return (p{textp_ = textp', current_title_ = title}, rect)

draw rs wd RepaintAll p@(RefGraphPainter refgraph _) = do
  graphdata <- readIORef refgraph
  let colors = graphColorTable wd
      WidgetAttributes sz pos  _ bg _ _ = attr_ wd
      (Size ws hs, Size x0 y0) = (sz, pos)
  let colorTable = map toColor colors
  bounds <- withDraw rs $ \d -> do
        drawRect (display_ rs) d bg x0 y0 ws hs
        let samp = transpose . fmap (scaleG hs . reverse . tail . scanl (+) 0) . take ws . exportGraph $ graphdata
        let segments = map (map (makeSegment y0 hs) . filter ((/=0) . snd)
                            . zip [x0+ws-1,x0+ws-2..]) samp
        mapM_ (drawColorSegment rs) $ zip segments colorTable
        -- FIXME: update only changed part
        return $ Just $ getBounds wd
  return (p, bounds)

-- catch all
draw rs wd _ p = return (p, Nothing)


type WindowPainter = (RenderState, [(Widget, Painter)])

makeBar :: Display -> IORef String -> Bar -> IO (Maybe (WindowPainter, [UpdaterDef]))
makeBar dpy titleRef barDef@(Bar _ _ screen _ _) = do

  let scr = defaultScreen dpy
  dimensions <- case screen of
       DefaultScreen -> return $ Just (0, 0, displayWidth dpy scr, displayHeight dpy scr)
       XineramaScreen x -> do
          xiscr <- maybe Nothing (find (\s -> xsi_screen_number s == fi x)) <$> xineramaQueryScreens dpy
          return $ fmap (\xi -> (xsi_x_org xi, xsi_y_org xi, fi (xsi_width xi), fi (xsi_height xi))) xiscr

  print $ "dimensions: " ++ show dimensions
  case dimensions of
    Nothing -> return Nothing
    Just d -> Just <$> makeBar' dpy titleRef barDef d

makeBar' dpy titleRef (Bar bg height screen gravity wds) (scX, scY, scWidth, scHeight) = do
  -- left, right, top, bottom,
  -- left_start_y, left_end_y, right_start_y, right_end_y,
  -- top_start_x, top_end_x, bottom_start_x, bottom_end_x
  let (y, strutValues) = if gravity == GravityTop
      then (scY,    [0, 0, fi scY + fi height, 0,
                     0, 0, 0, 0,
                     fi scX, fi scX + fi scWidth - 1, 0, 0])
      else (scY + fi scHeight - fi height,
                    [0, 0, 0, fi scY + fi height,
                     0, 0, 0, 0,
                     0, 0, fi scX, fi scX + fi scWidth - 1])

  let scr = defaultScreen dpy
  rootwin <- rootWindow dpy scr
  w <- createWindow dpy rootwin
                    (fi scX) (fi y) (fi scWidth) (fi height)
                    0 copyFromParent inputOutput (defaultVisual dpy scr) 0 nullPtr
  gc <- createGC dpy w
  buf <- createPixmap dpy w (fi scWidth) (fi height) (defaultDepth dpy scr)

  let rs = RenderState { display_ = dpy, window_ = w, buffer_ = buf, gc_ = gc,
                         windowWidth_ = fi scWidth, windowHeight_ = height,
                         windowBackground_ = bg,
                         windowPos_ = (fi scX, fi y, fi scX, fi scY, fi scWidth, fi scHeight),
                         titleRef_ = titleRef} -- FIXME: cleanup

  strutPartial <- internAtom dpy "_NET_WM_STRUT_PARTIAL" False
  changeProperty32 dpy w strutPartial cARDINAL propModeReplace strutValues
  strut <- internAtom dpy "_NET_WM_STRUT" False
  changeProperty32 dpy w strut cARDINAL propModeReplace (take 4 strutValues)

  dockAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_DOCK" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi dockAtom]

  xrrSelectInput dpy w rrScreenChangeNotifyMask
  windowMapAndSelectInput dpy w $ exposureMask
                              .|. structureNotifyMask
                              .|. buttonPressMask
                              .|. enterWindowMask
                              .|. leaveWindowMask
                              .|. pointerMotionMask

  wds' <- catMaybes <$> mapM removeBroken wds
  let widgets = layoutWidgets Horizontal (Size (fi scWidth) height) wds'
  initWidgets rs widgets


makeTooltip :: RenderState -> Widget -> Tooltip -> IO (WindowPainter, [UpdaterDef])
makeTooltip parent_rs pwd tip = do
  let RenderState { display_ = dpy,
                    titleRef_ = titleRef} = parent_rs
  let Tooltip bg tsz@(Size width height) orien widgets = tip
  let (px, py, scX, scY, scWidth, scHeight) = windowPos_ parent_rs

  let WidgetAttributes sz pos _ _ _ _ = attr_ pwd
  let wpos = Size px py
  let place = if y_ wpos == scY
                 then wpos + pos + sz * dir Vertical - half (tsz-sz) * dir Horizontal - Size 0 0
                 else wpos + pos - half (tsz-sz) * dir Horizontal - tsz * dir Vertical + Size 0 0
  let x = max scX $ min (x_ place) (scX + scWidth - width)
  let scr = defaultScreen dpy

  print $ "Enter! Creating Window " ++ show place ++ " wpos " ++ show wpos ++ " size " ++ show tsz

  let visual = defaultVisual dpy scr
      attrmask = cWOverrideRedirect
  w <- allocaSetWindowAttributes $ \attributes -> do
         set_override_redirect attributes True
         createWindow dpy (rootWindowOfScreen (defaultScreenOfDisplay dpy))
                    (fi x) (fi $ y_ place)
                    (fi width) (fi height) 0 copyFromParent
                    inputOutput visual attrmask attributes

  tooltopAtom <- internAtom dpy "_NET_WM_WINDOW_TYPE_TOOLTIP" False
  winType <- internAtom dpy "_NET_WM_WINDOW_TYPE" False
  changeProperty32 dpy w winType aTOM propModeReplace [fi tooltopAtom]

  gc <- createGC dpy w
  setLineAttributes dpy gc 1 lineSolid capRound joinRound

  buf <- createPixmap dpy w (fi width) (fi height) (defaultDepth dpy scr)

  windowMapAndSelectInput dpy w (structureNotifyMask .|. exposureMask)

  let rs = RenderState dpy w buf gc width height bg
                        (x, y_ place, scX, scY, scWidth, scHeight)
                        titleRef
  initWidgets rs widgets

data GraphKey = GraphKey GraphType TimeScale Period deriving (Eq, Ord, Generic, NFData, Show)
type GraphValue = ((Int, Sampler), GraphData)
type Graphs a = M.Map GraphKey a
type GlobalGraphs = Graphs GraphValue

aRs :: [WindowPainter] -> RenderState
aRs = fst . head

extractGraphInfo :: RefreshType -> Widget -> Maybe (GraphKey, Int) -- width
extractGraphInfo refreshType (Graph attr graph@(GraphDef typ tscale refreshType2) _ rate) =
  let Size ws _ =  size attr
  in if refreshType == refreshType2
       then Just (GraphKey typ tscale rate, ws)
       else Nothing

extractGraphInfo Always NetStatus {refreshRate_ = r, netdev_ = n} =
  Just (GraphKey (Net n) LinearTime r, 20)

extractGraphInfo _ _ = Nothing


makeGlobalGraphs :: [WindowPainter] -> Graphs Int
makeGlobalGraphs wins = do
  let leafWidgets Frame{ children = c} = concatMap leafWidgets c
      leafWidgets w = [w]
      tooltipWidgets (Tooltip bg tsz orien widgets) = widgets

      widgets = concatMap (concatMap (leafWidgets . fst) . snd) wins
      tooltips = mapMaybe (mbtooltip . attr_) widgets
      ttWidgets = concatMap (concatMap leafWidgets . tooltipWidgets) tooltips
      globalGraphs = mapMaybe (extractGraphInfo Always) widgets
                  ++ mapMaybe (extractGraphInfo WhenVisible) widgets
                  ++ mapMaybe (extractGraphInfo Always) ttWidgets
      hidden = mapMaybe (extractGraphInfo WhenVisible) ttWidgets
      -- pick longest from all graphs
      allLongest = M.fromListWith max $ globalGraphs ++ hidden
      global = M.fromList globalGraphs
  M.intersection allLongest global


changeGraphs graphs wins = do
  let targetGraphDefs = makeGlobalGraphs wins
  let newGraphDefs = M.difference targetGraphDefs graphs
  newGraphs <- M.fromAscList <$> mapM populateGraph (M.toAscList newGraphDefs)
  return $ M.intersection (M.union graphs newGraphs) targetGraphDefs
  where
    populateGraph :: (GraphKey, Int) -> IO (GraphKey, GraphValue)
    populateGraph (key@(GraphKey typ tscale period), ws) = do
      sampler <- makeSampler typ
      return (key, ((ws, sampler), makeGraph tscale ws (layers typ)))


-- Paint function, global timers, temporary timers
makeWidget :: RenderState -> Widget -> IO (Painter, Maybe UpdaterDef)

makeWidget rs wd@Label { label_ = msg } = do
  textp <- makeTextPainter rs (tattr_ wd)
  return (StaticTextPainter textp [msg], Nothing)

makeWidget rs wd@Clock { refreshRate_ = rate } = do
  tz <- case tz_ wd of
       LocalTimeZone -> localTimezone
       OtherTimeZone z -> otherTimezone z
  makeUpdatingTextWidget rs wd rate $ ClockUpdater (fmt_ wd) tz

makeWidget rs wd@Title {} = do
  textp <- makeTextPainter rs (tattr_ wd)
  return (RefTitlePainter textp "?", Nothing)

makeWidget rs wd@(Graph attr (GraphDef typ tscale refresh_type) colors rate) = do
  refgraph <- newIORef $ LinearGraph [] -- placeholder
  makeUpdatingWidget rate RefGraphPainter refgraph $ GraphUpdater (GraphKey typ tscale rate)

makeWidget rs wd@(CpuBars attr colors rate) = do
  refgraph <- newIORef $ LinearGraph [] -- placeholder
  makeUpdatingWidget rate RefGraphPainter refgraph $ CpuBarsUpdater []

makeWidget rs wd@NetStatus { netdev_ = netdev, refreshRate_ = rate} = do
  let key = fst $ fromJust $ extractGraphInfo Always wd

  makeUpdatingTextWidget rs wd rate $ NetStatusUpdater key


makeWidget rs wd@CpuTop {refreshRate_ = rate} = do
  avg <- loadavg
  ts <- getCurrentTime
  procs <- pickCpuUsage
  makeUpdatingTextWidget' [avg] rs wd rate $ CpuTopUpdater (ts, procs)

makeWidget rs wd@MemStatus {refreshRate_ = rate} = do
  makeUpdatingTextWidget rs wd rate MemStatusUpdater

makeWidget rs wd@BatteryStatus {batteryName_ = n, refreshRate_ = rate} = do
  makeUpdatingTextWidget rs wd rate (BatteryStatusUpdater n)

makeWidget rs wd@BatteryRate {batteryName_ = n, refreshRate_ = rate} = do
  makeUpdatingTextWidget rs wd rate (BatteryRateUpdater n)

makeWidget rs wd@Trayer {} = do
    let (pos, sz) = (position $ attr_ wd, size $ attr_ wd)
        cmd = trayerCmd (windowHeight_ rs) $ windowWidth_ rs - x_ pos - x_ sz
    print ("trayer cmd ", cmd)
    handle <- runCommand cmd
    return (NoPainter, Nothing)

makeWidget rs wd = do
   print $ "Not implemented: " ++ show rs
   return (NoPainter, Nothing)


type TooltipInfo = (RenderState, Widget)

inbounds :: WidgetAttributes -> Size -> Bool
inbounds WidgetAttributes {size = (Size ws hs), position = (Size wx wy)} (Size x y) =
  x > wx && x < wx + ws && y > wy && y < wy + hs

mouseHitWds :: [(Widget, Painter)] -> Size -> Maybe Widget
mouseHitWds wds pos =
  let match (wd,_) = inbounds (attr_ wd) pos && (isJust . mbtooltip . attr_ $ wd)
   in fst <$> find match wds

mouseHitWins :: [WindowPainter] -> (Window, Maybe Size) -> Maybe TooltipInfo
mouseHitWins _ (w, Nothing) = Nothing
mouseHitWins wins (w, Just pos) =
  let matchWin (rs, _) = w == window_ rs
      matchWds (rs, wds) = mouseHitWds wds pos >>= \wd -> Just (rs, wd)
   in find matchWin wins >>= matchWds

maybeCopyArea rs Nothing = return ()
maybeCopyArea rs (Just (Rect x y width height)) = do
   let RenderState {display_ = dpy, buffer_ = buf, window_ = w, gc_ = gc} = rs
   copyArea dpy buf w gc (fi x) (fi y) (fi width) (fi height) (fi x) (fi y)
   sync dpy False


paintWindow :: Repaint -> WindowPainter -> IO WindowPainter
paintWindow repaint (rs, drawableWidgets) = do
   rootRect <- paintBackground rs repaint
   res <- mapM (drawWrap rs repaint ) drawableWidgets
   let (mbRects, drawableWidgets') = unzip res -- FIXME ($)
   let mbRect = combinedRects $ catMaybes (rootRect:mbRects)
   maybeCopyArea rs mbRect
   return (rs, drawableWidgets')
   where
        paintBackground rs RepaintUpdated = return Nothing
        paintBackground rs RepaintAll = do
          let RenderState dpy ww b gc w h bg _ _ = rs
          withDraw rs $ \d -> drawRect dpy d bg 0 0 w h
          return $ Just $ Rect 0 0 (fi w) (fi h)

        combinedRects [] = Nothing
        combinedRects (x:xs) = Just $ foldr mergeRect x xs
        drawWrap rs repaint (wd, p) = do
          (p', mbrect) <- draw rs wd repaint p
          return (mbrect, (wd, p'))


type SenderThreadId = (ThreadId, SenderX)


runSenderThread :: Window -> (IO () -> IO ()) -> IO SenderThreadId
runSenderThread w task = do
  sender <- makeSenderX
  tid <- forkIO $ task (sendX sender w)
  return (tid, sender)


killSenderThread :: SenderThreadId -> IO ()
killSenderThread (tid, SenderX dpy _) = do
  killThread tid
  closeDisplay dpy


type UpdaterThreadState = (NominalDiffTime, -- tm
                          GlobalGraphs,
                          [(Updater, Period)], -- global
                          [(Updater, Period)]) -- tooltip

updateStep :: IORef UpdaterThreadState -> IO () -> IO ()
updateStep ref onupdated = do
    let min_period [] = 3600 -- empty thread doing nothing
        min_period timers = minimum $ map snd timers
    state@(tm, graphs, global_timers, tooltip_timers) <- readIORef ref
    let mp = min_period $ global_timers ++ tooltip_timers
    print $ "Min refresh rate: " ++ show mp 
    -- initialize tooltip refs from the current graphs and ignore state changes
    mapM_ (updateOne graphs 0 (tm-mp)) tooltip_timers
    iterateM_ (updateStep' onupdated ref mp) state

updateStep' :: IO () -> IORef UpdaterThreadState -> NominalDiffTime -> UpdaterThreadState -> IO UpdaterThreadState
updateStep' onupdated ref min_period (tm, graphs, global_timers, tooltip_timers) = do
    time <- getCurrentTime >>= \t -> return $ diffUTCTime t epoch :: IO NominalDiffTime
    let actual_tm = fi (truncate (time / min_period)) * min_period

    let next_tm = tm + min_period
    next_tm' <- if actual_tm > next_tm + min_period
       then do
          print $ "timer behind schedule by " ++ show (actual_tm - next_tm)
          return actual_tm
       else return next_tm

    graphs' <- M.fromAscList <$> mapM (updateOneGraph (tm-min_period) (next_tm'-min_period)) (M.toAscList graphs)
    (updated_g, global_timers') <- unzip <$> mapM (updateOne graphs (tm-min_period) (next_tm'-min_period)) global_timers
    (updated_t, tooltip_timers') <- unzip <$> mapM (updateOne graphs (tm-min_period) (next_tm'-min_period)) tooltip_timers
    when (or updated_g || or updated_t) onupdated

    actual_tm' <- getCurrentTime >>= \t -> return $ diffUTCTime t epoch :: IO NominalDiffTime
    let dt = next_tm' - actual_tm'
    when (dt > 0) $ threadDelay $ truncate (1000000 * dt)

    let state' = (next_tm', graphs', global_timers', tooltip_timers')
    state' `deepseq` writeIORef' ref state'
    return state'


timeForUpdate tm next_tm period =
    let a = truncate (tm / period) :: Int
        b = truncate (next_tm / period) :: Int
    in a /= b


updateOne :: GlobalGraphs -> NominalDiffTime -> NominalDiffTime -> (Updater, Period) -> IO(Bool, (Updater, Period))
updateOne graphs tm next_tm (mod, period) = do
    if timeForUpdate tm next_tm period
        then update graphs mod >>= \m' -> return (True, (m', period))
        else return (False, (mod, period))


updateOneGraph :: NominalDiffTime -> NominalDiffTime -> (GraphKey, GraphValue) -> IO(GraphKey, GraphValue)
updateOneGraph tm next_tm v@(key@(GraphKey typ tscale period), ((ws, sampler), graphdata)) = do
    if timeForUpdate tm next_tm period
        then do
          (v, sampler') <- readSample sampler
          let graphdata' = updateGraph ws graphdata v
          return (key, ((ws, sampler'), graphdata'))
        else return v


initWidgets :: RenderState -> [Widget] -> IO (WindowPainter, [UpdaterDef])
initWidgets rs widgets = do
  res <- mapM (makeWidget rs) widgets
  let (drawables, mbTimers) = unzip res

  let timers = catMaybes mbTimers :: [ UpdaterDef ]
  return ((rs, zip widgets drawables), timers)


data MainState = MainState {
  windows_ :: [WindowPainter],
  mbtooltip_ :: Maybe TooltipInfo,
  ref_ :: IORef UpdaterThreadState,
  thread_ :: SenderThreadId,
  titleThread_ :: SenderThreadId}


handleEvent :: MainState -> IO (Maybe TooltipInfo, MainState)
handleEvent state@(MainState wins tooltipInfo _ _ _) = do
  let dpy = display_ $ aRs wins
  allocaXEvent $ \ev -> do

    nextEvent dpy ev
    event <- getEvent ev
    let mouseHit ww x y = mouseHitWins wins (ww, Just (Size (fi x) (fi y)))

    --print $ "Event: " ++ show event
    case event of
       ClientMessageEvent {ev_window = ww, ev_data = 0:_} -> do
            wins' <- mapM (paintWindow RepaintUpdated) wins
            return (tooltipInfo, state{windows_ = wins'})

       ExposeEvent { ev_window = w } -> do
            wins' <- mapM (paintWindow RepaintAll) wins
            return (tooltipInfo, state{windows_ = wins'})

       ButtonEvent {ev_x = x, ev_y = y, ev_window = ww} -> do
            let hit = mouseHit ww x y
            let cmd = hit >>= (onclick . attr_ . snd)
            print cmd
            when (isJust cmd) $ void $ runCommand (fromJust cmd)
            return (hit, state)

       MotionEvent {ev_x = x, ev_y = y, ev_window = ww} -> do
            return (mouseHit ww x y,  state)

       e@CrossingEvent {ev_x = x, ev_y = y, ev_window = ww} -> do
            let hit = if ev_event_type e == enterNotify
                 then mouseHit ww x y
                 else Nothing
            return (hit, state)
       e@RROutputChangeNotifyEvent  {} -> restartBars state
       e@RRScreenChangeNotifyEvent  {} -> restartBars state

       _ -> return (tooltipInfo, state)


mbChangeTooltip (tooltipInfo, state@(MainState wins prev_tooltipInfo ref tid titleTid)) =
  if prev_tooltipInfo == tooltipInfo
    then return state
    else do
      killSenderThread tid
      (dt, graphs, globalTimers, _) <- readIORef ref
      (wins', tooltipTimers) <- destroyTooltip prev_tooltipInfo wins >>= createTooltip tooltipInfo
      graphs' <- changeGraphs graphs wins'
      let state' = (dt, graphs', globalTimers, tooltipTimers)
      writeIORef' ref state'
      tid' <- runSenderThread (window_ $ aRs wins') $ updateStep  ref
      return $ MainState wins' tooltipInfo ref tid' titleTid


destroyTooltip Nothing wins = return wins
destroyTooltip (Just _) wins = do
  let (win:wins') = wins
  destroyWin win
  return wins'


createTooltip Nothing wins = return (wins, [])
createTooltip (Just (rs, wd)) wins =
  create rs wd (mbtooltip . attr_ $ wd) wins where
      create _ _ Nothing wins = return (wins, [])
      create rs wd (Just tooltip) wins = do
          (win, timers) <- makeTooltip rs wd tooltip
          return (win:wins, timers)


eventLoop :: MainState -> IO ()
eventLoop state = handleEvent state >>= mbChangeTooltip >>= eventLoop

destroyWin (rs, _) = do
  destroyWindow (display_ rs) (window_ rs)


makeTitleThread rs = do
  let RenderState { display_ = dpy, window_ = w, titleRef_ = titleRef} = rs
  let terminate msg = do
        print msg
        --exitSuccess
        exitImmediately ExitSuccess
        return "a"
  runSenderThread (window_ rs) $ \onupdated -> forever $ do
     title <- getLine `catchIOError` terminate
     writeIORef' titleRef title
     onupdated


initMainState graphs titleRef = do
  dpy <- openDisplay ""
  (wins, updaters) <- unzip . catMaybes <$> mapM (makeBar dpy titleRef) bars
  graphs' <- changeGraphs graphs wins
  ref <- newIORef (0, graphs', concat updaters, [])
  titleTid <- makeTitleThread $ aRs wins
  tid <- runSenderThread (window_ $ aRs wins) $ updateStep ref
  return (MainState wins Nothing ref tid titleTid)


destroyMainState (MainState wins mbtooltip ref tid titleTid) = do
  let dpy = display_ $ aRs wins
  killSenderThread titleTid
  killSenderThread tid
  mapM_ destroyWin wins
  closeDisplay dpy


restartBars :: MainState -> IO (Maybe TooltipInfo, MainState)
restartBars state@(MainState wins _ ref _ _) = do
  print "Restarting"
  let titleRef = titleRef_ (aRs wins)
  destroyMainState state
  (_, graphs, _, _) <- readIORef ref
  state' <- initMainState graphs titleRef
  print "Restarted"
  return (Nothing, state')


main :: IO ()
main = do
  xSetErrorHandler
  titleRef <- newIORef "?"
  initMainState M.empty titleRef >>= eventLoop
