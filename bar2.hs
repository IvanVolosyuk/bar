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
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xrender
import System.IO.Error
import Text.Printf

import DzenParse
import Icon

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

bar1 :: Bar
bar1 = Bar barHeight {-screen-} 0 GravityBottom [
        clock # TimeFormat "%R" #
            Width 60 # RightPadding 4 #
            LocalTimeZone # BackgroundColor "#181838" #
            clockTooltip,

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
           tooltip label #Message "MTV:  " #JustifyRight,
           tooltip label #Message "Local:  " #JustifyRight
                       ]
 ]

tooltip w = w #BackgroundColor "#FFFFC0" #TextColor "#000000" 
              #TopPadding 0 #BottomPadding 1 #LeftPadding 0 #RightPadding 1
              #SetFont "-*-courier new-*-r-normal-*-17-*-*-*-*-*-*-*"

tooltipClock = tooltip clock #TimeFormat "%a, %e %b %Y - %X"

data Attribute = Width Int | Height Int | LeftPadding Int | RightPadding Int
               | TopPadding Int | BottomPadding Int
               | TextColor Main.Color | BackgroundColor Main.Color
               | TimeFormat String | Message String | SetFont String

type Color = String
type Font = String
data Size = Size Int Int deriving Show
type Pos = Size

type OnClickCmd = String

data Gravity = GravityTop | GravityBottom deriving (Show, Eq)
data Orientation = Horizontal | Vertical deriving (Show, Eq)

data ClockTimeZone = LocalTimeZone | OtherTimeZone String deriving Show
data Justify = JustifyLeft | JustifyMiddle | JustifyRight deriving Show
data Padding = Padding Size Size deriving Show
data TextAttributes = TextAttributes Main.Color Justify Main.Font deriving Show
data WidgetAttributes = WidgetAttributes {
  size :: Size,
  position :: Pos,
  padding :: Padding,
  color :: Main.Color,
  onclick :: Maybe OnClickCmd,
  mbtooltip :: Maybe Tooltip } deriving Show


data Bar = Bar Int Int Gravity [Widget] deriving Show
data Tooltip = Tooltip Size Orientation [Widget] deriving Show

class Apply a where
  apply :: a -> Widget -> Widget

instance Apply ClockTimeZone where
  apply tz ww = ww { tz_ = tz }

instance Apply Justify where
  apply j ww = let TextAttributes c _ f = tattr_ ww in ww { tattr_ = TextAttributes c j f }

instance Apply Tooltip where
  apply tip ww = let WidgetAttributes ws x p c cmd _ = attr_ ww
                   in ww { attr_ = WidgetAttributes ws x p c cmd (Just tip) }

withAttr ww f = ww { attr_ = f (attr_ ww) }
withPadding ww f = withAttr ww $ \wa -> let WidgetAttributes ws x p c cmd tip = wa in
    WidgetAttributes ws x (f p) c cmd tip

instance Apply Attribute where
  apply (TextColor c) ww = let TextAttributes _ j f = tattr_ ww in ww { tattr_ = TextAttributes c j f }
  apply (SetFont f) ww = let TextAttributes c j _ = tattr_ ww in ww { tattr_ = TextAttributes c j f }

  apply (Width w) ww = let WidgetAttributes (Size _ h) x p c cmd tip = attr_ ww
                           in ww { attr_ = WidgetAttributes (Size w h) x p c cmd tip}
  apply (Height h) ww = let WidgetAttributes (Size w _) x p c cmd tip = attr_ ww
                           in ww { attr_ = WidgetAttributes (Size w h) x p c cmd tip}

  apply (LeftPadding l) ww = withPadding ww $ \p -> let Padding (Size _ t) pbr = p in Padding (Size l t) pbr
  apply (TopPadding t) ww = withPadding ww $ \p -> let Padding (Size l _) pbr = p in Padding (Size l t) pbr
  apply (RightPadding r) ww = withPadding ww $ \p -> let Padding plt (Size _ b) = p in Padding plt (Size r b)
  apply (BottomPadding b) ww = withPadding ww $ \p -> let Padding plt (Size r _) = p in Padding plt (Size r b)
  apply (BackgroundColor c) ww = withAttr ww $ \attr -> let WidgetAttributes ws x p _ cmd tip = attr in
                                                     WidgetAttributes ws x p c cmd tip
  apply (TimeFormat fmt) ww = ww { fmt_ = fmt }
  apply (Message s) ww = ww { label_ = s } 

instance Num Size where
  (+) (Size x0 y0) (Size x1 y1) = Size (x0 + x1) (y0 + y1)
  (-) (Size x0 y0) (Size x1 y1) = Size (x0 - x1) (y0 - y1)
  (*) (Size x0 y0) (Size x1 y1) = Size (x0 * x1) (y0 * y1)
  abs (Size x y) = Size (abs x) (abs y)
  signum (Size x y) = Size (signum x) (signum y)
  fromInteger a = Size (fi a) (fi a)

infixl 9 #
(#) :: (Apply a) => Widget -> a  -> Widget
(#) w a = apply a w

data Widget = Clock   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, fmt_ :: String, tz_ :: ClockTimeZone }
          | Label   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes, label_ ::  String }
          | Title   {attr_ :: WidgetAttributes, tattr_ :: TextAttributes }
          | Latency {attr_ :: WidgetAttributes, tattr_ :: TextAttributes }
          | Frame   {attr_ :: WidgetAttributes, orient_ :: Orientation, children_ :: [Widget]}
          deriving Show

defaultAttr :: WidgetAttributes
defaultAttr = WidgetAttributes (Size 5000 barHeight) 0 (Padding 1 1) "#181838" Nothing Nothing

defaultTAttr :: TextAttributes
defaultTAttr = TextAttributes "#C7AE86" JustifyMiddle "-*-*-medium-r-normal--15-*-*-*-*-*-iso10646-*" 

clock :: Widget
clock = Clock defaultAttr defaultTAttr "%R" LocalTimeZone

label :: Widget
label = Label defaultAttr defaultTAttr ""

title :: Widget
title = Title defaultAttr defaultTAttr # Width 4000 

latency :: Widget
latency = Latency defaultAttr defaultTAttr

frame :: Orientation -> [Widget] -> Widget
frame = Frame (WidgetAttributes (Size 5000 barHeight) 0 (Padding 0 0)
                               "#181838" Nothing Nothing)

makeFont :: RenderState -> TextAttributes -> IO XftFont
makeFont RenderState { display = dpy } (TextAttributes _ _ fontName) =
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

type DrawCallback = Maybe XftDraw -> IO ()
type DrawRef = IORef DrawCallback
type ControlChan = Chan ([Window] -> DrawCallback)
data DrawableWidget = DrawableWidget Widget DrawRef
data RenderState = RenderState {
  display :: Display,
  window :: Window,
  buffer :: Pixmap,
  gc_ :: GC,
  windowWidth :: Int,
  windowHeight :: Int
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
  mouse_ :: Maybe Size
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

withDrawWidget :: RenderState -> Maybe XftDraw -> WidgetAttributes -> (XftDraw -> IO ()) -> IO ()
withDrawWidget rs globalDraw attr action =
  case globalDraw of
    Just d -> action d
    Nothing -> withDraw rs $ \d -> do
        let WidgetAttributes (Size width height) (Size x y) _ _ _ _ = attr
        action d
        copyToWindow rs (fi x) (fi y) (fi width) (fi height)
        sync (display rs) False


withColor :: Display -> String -> (XftColor -> IO ()) -> IO ()
withColor dpy = withXftColorName dpy vis colormap where
  (vis, colormap) = visualColormap dpy

drawStr :: Display -> WidgetAttributes -> TextAttributes
             -> XftFont -> XftDraw -> Int -> String -> IO Int
drawStr dpy attr tattr font d yoff msg = do
  let WidgetAttributes sz pos  _ bg _ _ = attr
  let TextAttributes fg justify _ = tattr

  glyphInfo <- xftTextExtents dpy font msg
  let Size ws hs = sz
  let Size x y = pos
  let [dx, dy, twidth] = map ($ glyphInfo) [
       xglyphinfo_x, xglyphinfo_y, xglyphinfo_width]
  let x' = x + case justify of
                  JustifyLeft -> dx
                  JustifyMiddle -> (ws - twidth) `div` 2
                  JustifyRight ->  ws - twidth
  let y' = y + ((hs + dy) `div` 2) + yoff
  _ <- xftDrawSetClipRectangles d 0 0 [Rectangle (fi x) (fi y) (fi ws) (fi hs)]
  withColor dpy bg $ \c -> xftDrawRect d c x y ws hs
  withColor dpy fg $ \c -> xftDrawString d c font x' y' msg
  return $ yoff + barHeight

data DrawableMessage = DrawableText XftFont MbColor MbColor String Size Size Size 
                     | DrawableIcon

drawMessage :: Display -> XftDraw -> DrawableMessage -> IO ()
drawMessage dpy d (DrawableText font fg bg msg sz pos tpos) = do
  let (Size ws hs, Size x y, Size tx ty) = (sz, pos, tpos)
  forM_ bg $ \c -> withColor dpy c $ \cc -> xftDrawRect d cc x y ws hs
  forM_ fg $ \c -> withColor dpy c $ \cc -> xftDrawString d cc font tx ty msg

drawMessage dpy d DrawableIcon =
  return ()

drawSomething :: RenderState -> WidgetAttributes -> [DrawableMessage] -> Maybe XftDraw -> IO ()
drawSomething rs wa msgs globalDraw = withDrawWidget rs globalDraw wa
               $ \d -> mapM_ (drawMessage (display rs) d) msgs

drawStringWidget :: RenderState -> Widget -> XftFont -> [String] -> Maybe XftDraw -> IO ()
drawStringWidget rs@RenderState { display = dpy } wd font strings globalDraw = do
  let draw = drawStr dpy (attr_ wd) (tattr_ wd) font
  withDrawWidget rs globalDraw (attr_ wd) $ \d -> foldM_ (draw d) 0 strings

makeTextPainter rs wd = do
  fn <- makeFont rs (tattr_ wd)
  return $ drawStringWidget rs wd fn

repaint :: (RenderState, ControlChan, DrawRef) -> DrawCallback -> IO ()
repaint (rs, ch, ref) drawable = do
  tid <- myThreadId
  let drawable' winids d = do
          let w = window rs
          if window rs `elem` winids then drawable d else killThread tid
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

reserve :: WidgetAttributes -> Size -> Size -> Orientation -> (WidgetAttributes, Size)
reserve wa wpos wsz ort =
  let WidgetAttributes sz _ p@(Padding plt prb) bg cmd tip = wa
      flp o = if o == Horizontal then Vertical else Horizontal
      dir o = if o == Horizontal then Size 1 0 else Size 0 1
      clamp (Size w h) = Size (max 0 w) (max 0 h)
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

paintWindow :: WindowState -> IO ()
paintWindow (WindowState rs wds bg) = do
  let RenderState { display = dpy, windowWidth = width,
                    windowHeight = height } = rs
  withDraw rs $ \d -> do
    printf "%d x %d\n" width height
    withColor dpy bg $ \c -> xftDrawRect d c (0::Int) (0::Int) width height
    mapM_ (draw $ Just d) wds
    copyToWindow rs 0 0 (fi width) (fi height)
    sync dpy False where
      draw :: Maybe XftDraw -> DrawableWidget -> IO ()
      draw d (DrawableWidget _ ref) = liftIO (readIORef ref) >>= \cb -> cb d

windowMapAndSelectInput :: Display -> Window -> Word64 -> IO ()
windowMapAndSelectInput dpy w mask = do
  selectInput dpy w mask
  mapWindow dpy w
  sync dpy False
  flush dpy

createTooltip :: RenderState -> Widget -> Tooltip -> RW ()
createTooltip parent_rs parent tip = do
  let dpy = display parent_rs
  let Tooltip (Size width height) orien widgets = tip
  liftIO $ print "Enter! Creating Window"
  let barWidth = 1920 -- FIXME
  let pos = 1920 - 300

  let scr = defaultScreen dpy
  let visual = defaultVisual dpy scr
  let winPos = min barWidth  (pos + (width `div` 2)) - width
      attrmask = cWOverrideRedirect
  w <- liftIO $ allocaSetWindowAttributes $ \attributes -> do
         set_override_redirect attributes True
         createWindow dpy (defaultRootWindow dpy) (fi winPos) (fi barHeight)
                    (fi width) (fi height) 0 copyFromParent
                    inputOutput visual attrmask attributes

  tooltopAtom <- liftIO $ internAtom dpy "_NET_WM_WINDOW_TYPE_TOOLTIP" False
  winType <- liftIO $ internAtom dpy "_NET_WM_WINDOW_TYPE" False
  liftIO $ changeProperty32 dpy w winType aTOM propModeReplace [fi tooltopAtom]

  gc <- liftIO $ createGC dpy w
  liftIO $ setLineAttributes dpy gc 1 lineSolid capRound joinRound

  buf <- liftIO $ createPixmap dpy w (fi width) (fi height) (defaultDepth dpy scr)

  liftIO $ windowMapAndSelectInput dpy w (structureNotifyMask .|. exposureMask)

  let rs = RenderState dpy w buf gc width height
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
  get >>= \s -> put $ s { mouse_ = next }

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
  liftIO $ mapM_ (\win -> when (w == (window . rs_) win) $ paintWindow win) allwins

handleEvent ClientMessageEvent {} = do
  ch <- reader controlChan
  allWinIds <- map (window . rs_) <$> allWindows
  -- join (readChan ch)
  action  <- liftIO $ readChan ch
  liftIO $ action allWinIds Nothing

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
makeBar dpy controlCh (Bar height screenNum gravity wds) = do
  cc <- get
  put False
  liftIO $ do
    let scr = fi screenNum
    let visual = defaultVisual dpy scr
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
    
    w <- createWindow dpy (defaultRootWindow dpy) 0 (fi y) (fi width) (fi height)
                      0 copyFromParent inputOutput visual 0 nullPtr
    gc <- createGC dpy w
    buf <- createPixmap dpy w (fi width) (fi height) (defaultDepth dpy scr)
    
    let rs = RenderState { display = dpy, window = w, buffer = buf, gc_ = gc,
                                   windowWidth = width, windowHeight = height }
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

  let ro = ROState { windows = wins, controlChan = controlCh }
  let rw = RWState { tooltip_ = Nothing, mouse_ = Nothing }

  _ <- runReaderT (runStateT eventLoop rw) ro
  return ()

