import Control.Concurrent
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State
import Data.Bits
import Data.IORef
import Data.List
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

import DzenParse
import Icon
import Top

barHeight :: Int
barHeight = 24

marginTop :: Int
marginTop = 1

marginBottom :: Int
marginBottom = 1

barBackground :: String
barBackground = "#BEBEBE"

tooltipBackground :: String
--tooltipBackground = "#FFFFC0"
tooltipBackground = "#505050"

bars :: [Bar]
bars = [bar1, bar2]
-- bars = [bar1]

bar1 :: Bar
bar1 = Bar barHeight {-screen-} 0 GravityBottom [
        clock # TimeFormat "%R" #
            Width 60 # RightPadding 4 #
            LocalTimeZone # BackgroundColor "#181838" #
            clockTooltip,
        cpu # Width 120 #cpuTooltip,

        title # LeftPadding 2 # RightPadding 2 #
                BackgroundColor barBackground #
                JustifyLeft # TextColor "#000000"
      ]

bar2 :: Bar
bar2 = Bar (barHeight*2) {-screen-} 0 GravityTop [
        clock # TimeFormat "%R" #
            Width 60 # RightPadding 4 #
            LocalTimeZone # BackgroundColor "#181838" #
            clockTooltip,

        title # LeftPadding 2 # RightPadding 2 #
                BackgroundColor barBackground #
                JustifyLeft # TextColor "#000000"
      ]

clockTooltip = Tooltip (Size 460 (barHeight * 2)) Horizontal [
        frame Vertical [
                         tooltipClock #OtherTimeZone "America/Los_Angeles",
                         tooltipClock
                       ] #Width 340,
        frame Vertical [
           tooltipLabel #Message "MTV: " #JustifyRight,
           tooltipLabel #Message "Local: " #JustifyRight
                       ]
 ]

cpuTooltip = Tooltip (Size 300 (4*barHeight)) Horizontal [
     tooltipText cpuTop #Height (4*barHeight) #Width 280,
     tooltip cpu #RefreshRate 0.1 # Width 20 #LinearTime
     ]

tooltip w = w #BackgroundColor "#FFFFC0"
              #TopPadding 0 #BottomPadding 1 #LeftPadding 0 #RightPadding 1

tooltipText w = tooltip w  #TextColor "#000000"
              #SetFont "-*-courier new-*-r-normal-*-17-*-*-*-*-*-*-*"

tooltipClock = tooltipText clock #TimeFormat "%a, %e %b %Y - %X"
tooltipLabel = tooltipText label

data Attribute = Width Int | Height Int | LeftPadding Int | RightPadding Int
               | TopPadding Int | BottomPadding Int
               | TextColor Main.Color | BackgroundColor Main.Color
               | TimeFormat String | Message String | SetFont String
               | RefreshRate Double

type Color = String
type Font = String
data Size = Size {x_ :: Int, y_ :: Int} deriving Show
type Pos = Size

type OnClickCmd = String

data Gravity = GravityTop | GravityBottom deriving (Show, Eq)
data Orientation = Horizontal | Vertical deriving (Show, Eq)

data ClockTimeZone = LocalTimeZone | OtherTimeZone String deriving Show
data Justify = JustifyLeft | JustifyMiddle | JustifyRight deriving Show
data Padding = Padding Size Size deriving Show
data TextAttributes = TextAttributes Main.Color Justify Main.Font Int deriving Show
data WidgetAttributes = WidgetAttributes {
  size :: Size,
  position :: Pos,
  padding :: Padding,
  color :: Main.Color,
  onclick :: Maybe OnClickCmd,
  mbtooltip :: Maybe Tooltip } deriving Show

-- int = n linear points before 2x compression
data TimeScale = LinearTime | LogTime Int deriving Show


data Bar = Bar Int ScreenNumber Gravity [Widget] deriving Show
data Tooltip = Tooltip Size Orientation [Widget] deriving Show

class Apply a where
  apply :: a -> Widget -> Widget

instance Apply ClockTimeZone where
  apply tz ww = ww { tz_ = tz }

instance Apply Justify where
  apply j ww = let TextAttributes c _ f hs = tattr_ ww in ww { tattr_ = TextAttributes c j f hs}

instance Apply TimeScale where
  apply t ww = ww { timeScale_ = t }

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

data Widget = Clock   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, fmt_ :: String, tz_ :: ClockTimeZone }
          | Label   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, label_ ::  String }
          | Title   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes }
          | CpuTop  {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, refreshRate :: Double}
          | Latency {attr_ :: WidgetAttributes, tattr_ :: TextAttributes }
          | Frame   {attr_ :: WidgetAttributes, orient_ :: Orientation, children_ :: [Widget]}
          | CpuGraph{attr_ :: WidgetAttributes, colorTable :: [String], refreshRate :: Double, timeScale_ :: TimeScale}
          deriving Show

defaultAttr :: WidgetAttributes
defaultAttr = WidgetAttributes (Size 5000 barHeight) 0 (Padding 1 1) "#181838" Nothing Nothing

defaultTAttr :: TextAttributes
defaultTAttr = TextAttributes "#C7AE86" JustifyMiddle "-*-*-medium-r-normal--15-*-*-*-*-*-iso10646-*" barHeight

clock :: Widget
clock = Clock defaultAttr defaultTAttr "%R" LocalTimeZone

cpu :: Widget
cpu = CpuGraph defaultAttr ["#70FF70", "#FF8080", "#F020F0", "#3030FF"] 1 (LogTime 5)

label :: Widget
label = Label defaultAttr defaultTAttr ""

title :: Widget
title = Title defaultAttr defaultTAttr # Width 4000

latency :: Widget
latency = Latency defaultAttr defaultTAttr

cpuTop :: Widget
cpuTop = CpuTop defaultAttr defaultTAttr 3 # JustifyLeft

frame :: Orientation -> [Widget] -> Widget
frame = Frame (WidgetAttributes (Size 5000 barHeight) 0 (Padding 0 0)
                               "#181838" Nothing Nothing)

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
type ControlChan = Chan ([Window] -> DrawCallback)
data DrawableWidget = DrawableWidget Widget DrawRef
data RenderState = RenderState {
  display :: Display,
  window :: Window,
  buffer :: Pixmap,
  gc_ :: GC,
  windowWidth :: Int,
  windowHeight :: Int,
  screenNum :: ScreenNumber
}

data WindowState = WindowState {
  rs_ :: RenderState,
  widgets_ :: [DrawableWidget],
  winBackground :: String
}

data ROState = ROState {
  windows :: [WindowState],
  controlChan :: ControlChan
}

data RWState = RWState {
  tooltip_ :: Maybe WindowState,
  mouse_ :: Maybe Size,
  iconCache_ :: IconCache
}

type RW a = StateT RWState (ReaderT ROState IO) a

instance Show DrawableWidget where
  show (DrawableWidget x _) = "Drawable " ++ show x

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

copyToWindow :: RenderState -> Int -> Int -> Int -> Int -> IO ()
copyToWindow RenderState {display = dpy, buffer = buf, window = w, gc_ = gc} x y width height =
  liftIO $ copyArea dpy buf w gc (fi x) (fi y) (fi width) (fi height) (fi x) (fi y)

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

liftIconCache :: DrawCall () -> RW ()
liftIconCache f = do
  icons <- iconCache_ <$> get
  icons' <- liftIO $ execStateT f icons
  get >>= \s -> put s { iconCache_ = icons' }

getIcon :: Window -> DrawCall CachedIcon
getIcon w = do
  icons <- get
  (icons', img) <- liftIO $ loadIconImage icons w
  put icons'
  return img

withDrawWidget :: RenderState -> Maybe XftDraw -> WidgetAttributes -> (XftDraw -> DrawCall ()) -> DrawCall ()
withDrawWidget rs globalDraw attr action =
  case globalDraw of
    Just d -> action d
    Nothing -> withDrawState rs $ \d -> do
        let WidgetAttributes (Size width height) (Size x y) _ _ _ _ = attr
        action d
        liftIO $ copyToWindow rs (fi x) (fi y) (fi width) (fi height)
        liftIO $ sync (display rs) False


withColor :: Display -> String -> (XftColor -> IO ()) -> IO ()
withColor dpy = withXftColorName dpy vis colormap where
  (vis, colormap) = visualColormap dpy


drawMessage rs attr tattr font d off (Text fg bg msg) = do
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
  return $ Size (xoff + txoff) yoff

drawMessage rs attr tattr font d off (IconRef wid) = do
  let WidgetAttributes sz pos  _ wbg _ _ = attr
  let (Size ws _, Size x y, Size xoff yoff) = (sz, pos, off)
  let TextAttributes _ _ _ ths = tattr
  CachedIcon width height img <- getIcon $ fi wid
  liftIO $ putImage (display rs) (buffer rs) (gc_ rs)
           img 0 0 (fi $ x + xoff) (fi $ y + yoff + ((ths - height) `div` 2)) (fi width) (fi height)
  return $ Size (xoff + width) yoff

drawMessages :: RenderState -> WidgetAttributes -> TextAttributes
             -> XftFont -> XftDraw -> Int -> String -> DrawCall Int
drawMessages rs attr tattr font d yoff msg = do
  let TextAttributes _ _ _ ths = tattr
  foldM_ (drawMessage rs attr tattr font d) (Size 0 yoff) (parseLine msg)
  return $ yoff + ths

drawStringWidget :: RenderState -> Widget -> XftFont -> [String] -> Maybe XftDraw -> DrawCall ()
drawStringWidget rs@RenderState { display = dpy } wd font strings globalDraw = do
  let draw = drawMessages rs (attr_ wd) (tattr_ wd) font
  withDrawWidget rs globalDraw (attr_ wd) $ \d -> do
    let WidgetAttributes sz pos  _ wbg _ _ = attr_ wd
    let (Size ws hs, Size x y) = (sz, pos)
    _ <- liftIO $ xftDrawSetClipRectangles d 0 0 [Rectangle (fi x) (fi y) (fi ws) (fi hs)]
    liftIO $ drawRect (display rs) d wbg x y ws hs
    foldM_ (draw d) 0 strings

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
  writeChan ch drawable'

runStatelessThread :: IO a -> IO ()
runStatelessThread action = void $ forkIO $ forever action

runThread :: a -> (a -> IO a) -> IO ()
runThread a f = void $ forkIO $ iterateM_ f a

buildWidget :: (RenderState, ControlChan, DrawRef) -> Widget -> IO ()

buildWidget (rs,_,ref) wd@Label { label_ = msg } = do
  draw <- makeTextPainter rs wd
  writeIORef ref $ draw [msg]

buildWidget ctx@(rs,_,_) wd@Latency {} = do
  draw <- makeTextPainter rs wd
  backChan <- newChan :: IO (Chan Char)
  ts <- getCurrentTime
  runThread ts $ \prev-> do
    ts <- getCurrentTime
    repaint ctx $ \d -> do
      draw [show $ diffUTCTime ts prev] d
      liftIO $ writeChan backChan '1'
    _ <- readChan backChan
    threadDelay 1
    return ts

buildWidget ctx@(rs,_,_) wd@Clock {} = do
  draw <- makeTextPainter rs wd
  tz <- case tz_ wd of
          LocalTimeZone -> localTimezone
          OtherTimeZone z -> otherTimezone z
  -- Cache expensive timezone computation
  runStatelessThread $ do
    msg <- formatClock (fmt_ wd) tz
    repaint ctx $ draw [msg]
    threadDelay 1000000

buildWidget ctx@(rs,_,_) wd@Title {} = do
  draw <- makeTextPainter rs wd
  thr <- myThreadId
  let makeAction thr result = case result of
        Left _ -> const $ liftIO $ killThread thr
        Right msg -> draw [msg]
  runStatelessThread $ makeAction thr <$> tryIOError getLine >>= repaint ctx

buildWidget ctx@(rs,_,_) wd@(CpuGraph attr colors refresh tscale) = do
  let WidgetAttributes sz pos  _ bg _ _ = attr
      (Size ws hs, Size x0 y0) = (sz, pos)
      readCPU = map read . tail. words . head . lines <$> readFile "/proc/stat"
  cpu <- readCPU
  let graph = makeGraph tscale ws (length cpu)
  let colorTable = map toColor colors

  runThread (cpu,graph) $ \(cpu',graph')-> do
    cpu'' <- readCPU
    let (user:nice:sys:idle:io:_) = zipWith (-) cpu'' cpu'
        (total:points) = reverse $ scanl (+) 0 [sys + io, nice, user, idle]
        sample = map ((`div` (if total == 0 then 1 else total)) . (*hs)) points
        graph'' = updateGraph graph' sample ws

    repaint ctx $ \gd ->
      withDrawWidget rs gd attr $ \d -> liftIO $ do
        drawRect (display rs) d bg x0 y0 ws hs
        -- drawSegments (Segment ) 0xFF8080
        let segments = map (map (makeSegment y0 hs) . filter ((/=0) . snd)
                            . zip [x0+ws-1,x0+ws-2..]) . exportGraph $ graph''
        mapM_ (drawColorSegment rs) $ zip segments  colorTable
    threadDelay $ round (1000000 * refresh)
    return (cpu'', graph'')


buildWidget ctx@(rs,_,_) wd@CpuTop {refreshRate = refresh} = do
  draw <- makeTextPainter rs wd
  let loadavg = foldl (\a b -> a++" "++b) "Load avg: " . take 3 . words <$> readFile "/proc/loadavg"
  let getDt newTs ts = (/1e12) . fromIntegral . fromEnum $ diffUTCTime newTs ts
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

data Graph = LinearGraph [[Int]] | LogGraph Int [[[Int]]]

makeGraph :: TimeScale -> Int -> Int -> Graph
makeGraph LinearTime ws l = LinearGraph $ replicate l $ replicate ws 0
makeGraph (LogTime n) ws l = LogGraph n $ replicate l $ replicate (1 + ws `div` n) []

updateGraph :: Graph -> [Int] -> Int -> Graph
updateGraph (LinearGraph g) s ws = LinearGraph $ map (\(n,o) -> n : take (ws-1) o) $ zip s g
updateGraph (LogGraph n g) s ws = LogGraph n $ zipWith updateLayer g s where
  updateLayer (vv:xs) v
    | length vv == (n+2) = let (a,b) = splitAt n vv in (v:a):updateLayer xs ((1 + sum b) `div` length b) 
    | otherwise = (v:vv):xs

exportGraph (LinearGraph g) = g
exportGraph (LogGraph n g) = map (concatMap (take' n)) g where
  take' n arr = let (a,b) = splitAt (n-1) arr in a ++ (case b of
       [] -> []
       _ -> [(1 + sum b) `div` length b])



makeSegment y0 height (x,y) = Segment x' y0' x' y1' where
  x' = fi x
  y0' = fi $ height - 1 + y0
  y1' = fi $ height - 1 + y0 - fi y

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

layoutWidget :: RenderState -> ControlChan -> Size -> Orientation -> Widget
                -> StateT ([DrawableWidget], Size) IO ()
layoutWidget rs ch wpos ort (Frame wa cort cwds) = do
  (out, wsz) <- get
  let (WidgetAttributes ws pos _ _ _ _, newpos) = reserve wa wpos wsz ort
  put (out, ws)
  mapM_ (layoutWidget rs ch pos cort) cwds
  out' <- fst <$> get
  put (out', newpos)

layoutWidget rs ch wpos ort wd = do
  (out, wsz) <- get
  let (wa', newpos) = reserve (attr_ wd) wpos wsz ort
      wd' = wd { attr_ = wa' }
  liftIO $ print wa'

  ref <- liftIO $ newIORef $ const $ return ()
  liftIO $ buildWidget (rs, ch, ref) wd'
  put (DrawableWidget wd' ref : out, newpos)

layoutWidgets :: RenderState -> ControlChan -> Size -> Orientation -> [Widget] -> IO [DrawableWidget]
layoutWidgets rs ch wsz orien wds = do
  (out, _) <- execStateT (mapM_ (layoutWidget rs ch (Size 0 0) orien) wds) ([], wsz)
  print $ head out
  return out

paintWindow :: WindowState -> RW ()
paintWindow (WindowState rs wds bg) = do
  let RenderState { display = dpy, windowWidth = width,
                    windowHeight = height } = rs
  liftIconCache $ withDrawState rs $ \d -> do
    liftIO $ printf "%d x %d\n" width height
    liftIO $ drawRect dpy d bg 0 0 width height
    mapM_ (draw $ Just d) wds
    liftIO $ copyToWindow rs 0 0 (fi width) (fi height)
    liftIO $ sync dpy False where
      draw :: Maybe XftDraw -> DrawableWidget -> DrawCall ()
      draw d (DrawableWidget _ ref) = liftIO (readIORef ref) >>= \cb -> cb d

windowMapAndSelectInput :: Display -> Window -> Word64 -> IO ()
windowMapAndSelectInput dpy w mask = do
  selectInput dpy w mask
  mapWindow dpy w
  sync dpy False
  flush dpy

createTooltip :: RenderState -> Widget -> Tooltip -> RW ()
createTooltip parent_rs pwd tip = do
  let dpy = display parent_rs
  let Tooltip sz@(Size width height) orien widgets = tip

  parent <- liftIO $ getWindowAttributes dpy (window parent_rs)
  let WidgetAttributes ws pos _ _ _ _ = attr_ pwd
  let wpos = Size (fi $ wa_x parent) (fi $ wa_y parent)
  let place = if y_ wpos == 0
                 then wpos + pos + ws * dir Vertical - half ws * dir Horizontal - Size 0 2
                 else wpos + pos - half ws * dir Horizontal - sz * dir Vertical + Size 0 2
  let screenWidth = fi $ displayWidth dpy (screenNum parent_rs)
  let x = max 2 $ min (x_ place) (screenWidth - width - 2)

  liftIO $ print $ "Enter! Creating Window " ++ show place ++ " wpos " ++ show wpos

  let scr = screenNum parent_rs
  let visual = defaultVisual dpy scr
      attrmask = cWOverrideRedirect
  w <- liftIO $ allocaSetWindowAttributes $ \attributes -> do
         set_override_redirect attributes True
         createWindow dpy (rootWindowOfScreen (screenOfDisplay dpy scr))
                    (fi x) (fi $ y_ place)
                    (fi width) (fi height) 0 copyFromParent
                    inputOutput visual attrmask attributes

  tooltopAtom <- liftIO $ internAtom dpy "_NET_WM_WINDOW_TYPE_TOOLTIP" False
  winType <- liftIO $ internAtom dpy "_NET_WM_WINDOW_TYPE" False
  liftIO $ changeProperty32 dpy w winType aTOM propModeReplace [fi tooltopAtom]

  gc <- liftIO $ createGC dpy w
  liftIO $ setLineAttributes dpy gc 1 lineSolid capRound joinRound

  buf <- liftIO $ createPixmap dpy w (fi width) (fi height) (defaultDepth dpy scr)

  liftIO $ windowMapAndSelectInput dpy w (structureNotifyMask .|. exposureMask)

  let rs = RenderState dpy w buf gc width height (screenNum parent_rs)
  ch <- reader controlChan
  widgets' <- liftIO $ layoutWidgets rs ch (Size width height) orien widgets
  get >>= \s -> put s { tooltip_ = Just $ WindowState rs widgets' tooltipBackground }

deleteTooltip :: RW ()
deleteTooltip = do
  s <- get
  let tip = tooltip_ s
  -- TODO: use some function
  case tip of
    Nothing -> return ()
    Just t -> do
      liftIO $ print "Destroy tooltip"
      liftIO $ destroyWindow (display . rs_ $ t) (window . rs_ $ t)
      put s { tooltip_ = Nothing }

condM_ :: [a] -> (a -> Bool) -> (a -> RW ()) -> RW ()
condM_ t cond action = forM_ t (\a -> when (cond a) $ action a)

allWindows = do
  wins <- reader windows
  tip <- tooltip_ <$> get
  return $ maybeToList tip ++ wins

inbounds :: WidgetAttributes -> Size -> Maybe Bool
inbounds WidgetAttributes {size = (Size ws hs), position = (Size wx wy)} (Size x y) =
        Just (x > wx && x < wx + ws && y > wy && y < wy + hs)

updateTooltip :: Window -> Maybe Size -> RW ()
updateTooltip ww next = do
  prev <- mouse_ <$> get
  get >>= \s -> put s { mouse_ = next }

  liftIO $ printf "%s ->%s\n" (show prev) (show next)
  let insideWA wa p = fromMaybe False (p >>= inbounds wa)
  let inside wd = insideWA (attr_ wd)

  let update rs (DrawableWidget wd _) =
       update' (inside wd prev) (inside wd next) rs wd where
        update' False True rs' wd' = do
               liftIO $ print $ "Create tooltip requested: " ++ show wd'
               forM_ (mbtooltip . attr_ $ wd') (createTooltip rs' wd')

        update' True False _ _ = deleteTooltip
        update' _ _ _ _ = return ()

  wins <- reader windows
  condM_ wins (\w -> (window . rs_ $ w) == ww) $ \win -> do
      let WindowState rs wds _ = win
      mapM_ (update rs) wds

handleEvent :: Event -> RW ()
handleEvent ExposeEvent { ev_window = w } = do
  wins <- reader windows
  tip <- tooltip_ <$> get
  let allwins = maybeToList tip ++ wins
  mapM_ (\win -> when (w == (window . rs_) win) $ paintWindow win) allwins

handleEvent ClientMessageEvent {} = do
  ch <- reader controlChan
  allWinIds <- map (window . rs_) <$> allWindows
  liftIO (readChan ch) >>= \act -> liftIconCache $ act allWinIds Nothing

handleEvent MotionEvent {ev_x = x, ev_y = y, ev_window = ww} = do
  let next = Just $ Size (fi x) (fi y)
  updateTooltip ww next

handleEvent CrossingEvent {ev_event_type = typ,
                           ev_window = ww,
                           ev_x = x, ev_y = y } = do
  let next = if typ == enterNotify then Just $ Size (fi x) (fi y) else Nothing
  updateTooltip ww next

handleEvent AnyEvent {ev_event_type = 14} = return () -- NoExpose

handleEvent event@_ =
  liftIO $ print $ "Unhandled event:" ++
    eventName event ++ ": " ++ show event

eventLoop :: RW ()
eventLoop = forever $ do
  dpy <- display . rs_ . head <$> reader windows
  event <- liftIO $ allocaXEvent $ \ev -> do
    liftIO $ nextEvent dpy ev
    getEvent ev

  handleEvent event

sendClientEvent :: Display -> Atom -> Window -> Atom -> IO ()
sendClientEvent d a w val = do
    allocaXEvent $ \e -> do
         setEventType e clientMessage
         setClientMessageEvent e w a 32 val currentTime
         sendEvent d w False structureNotifyMask e
    sync d False

copyChanToX :: Chan a -> Window -> IO ()
copyChanToX chan w = do
  chanCopy <- dupChan chan
  d <- openDisplay ""
  a <- internAtom d "BAR_UPDATE" False
  forever $ do
     _ <- readChan chanCopy
     sendClientEvent d a w 0 `catchIOError`  \x -> do
       print $ "Exception caught: " ++ show x
       sync d False

makeBar :: Display -> ControlChan -> Bar -> StateT Bool IO WindowState
makeBar dpy controlCh (Bar height scr gravity wds) = do
  cc <- get
  put False
  liftIO $ do
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
                                   screenNum = scr}
    when cc $ void $ forkOS $ copyChanToX controlCh w

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

    widgets <- layoutWidgets rs controlCh (Size width height) Horizontal wds
    return $ WindowState rs widgets barBackground

main :: IO ()
main = do
  xSetErrorHandler
  dpy <- openDisplay ""
  controlCh <- newChan :: IO ControlChan

  wins <- evalStateT (mapM (makeBar dpy controlCh) bars) True
  icons <- makeIconCache dpy

  let ro = ROState { windows = wins, controlChan = controlCh }
  let rw = RWState { tooltip_ = Nothing, mouse_ = Nothing, iconCache_ = icons }

  _ <- runReaderT (runStateT eventLoop rw) ro
  return ()

