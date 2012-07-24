import Data.Word
import Control.Monad.State
import Control.Monad
import System.Posix.Unistd
import Data.List
import Text.Printf
import Control.Applicative
import Data.Map ((!), Map, fromList)
import Data.Time
import Control.Concurrent
import Control.Concurrent.Chan
import System.IO
import System.Posix.IO
import IO
import System.Posix.Signals
import Graphics.X11.Xlib
import System.Posix.Process
import System.Process
import System.Directory (doesFileExist)
import Xpm
import Data.HashTable (hashString)

height = 22
padding = 4
graphBackgroundColor = "#181838"
cpuColorTable = ["#007F00", "#7F0000", "#600060", "#0000FF"]
batteryColorTable = ["#303060"]
memColorTable = ["#007F00", "#FF0000", "#0000FF"]
netColorTable = ["#0000FF", graphBackgroundColor, "#00FF00"]
netSilenceThreshold = 100

--          Object    refresh (sec)  position
layout= [ (emptySpace,      never,  L 10),
          (genTitle,            0,  L 0),
          (net "wlan0",         5,  R 20),
          (net "eth0",          5,  R 20),
       --   (batteryGraph,      300,  R 40),
          (battery,             5,  R 120),
          (mem,                60,  R 15),
          (cpu,                 2,  R 60),
          (clock,               2,  R 60)
         ]

layout0 = [( net "wlan0", 5, L 2)]

getScreenWidth = do
  x <- openDisplay ":0"
  return . fromIntegral . widthOfScreen . screenOfDisplay x $ defaultScreen x


debug = False

move :: Int -> String
color :: String -> String -> String
bar :: Int -> String
(move, color, bar) = if debug
  then (debugMove, debugColor, debugBar)
  else (dzenMove, dzenColor, dzenBar) where
  debugMove _ = " // "
  debugColor _ = printf "(%s)"
  debugBar = printf "%d "

  dzenMove = printf "^p(%d)"
  dzenColor = printf "^fg(%s)%s^fg()"
  dzenBar h = printf "^pa(;%d)^r(1x%d)" (height-h + 1) (h::Int)

split :: Char -> String -> [String]
split ch s =  case dropWhile (==ch) s of
  "" -> []
  s' -> word : split ch s''
    where (word, s'') = break (==ch) s'

strip :: String -> String
strip s = reverse . dropWhile p . reverse . dropWhile p $ s where
  p = (==' ')

split1 ch s = (x, tail xs) where
  (x,xs) = break (==ch) s

updateGraph samples sample = newSamples where
  newSamples = map (\(n,o) -> o++[n]) $ zip sample $ map (drop 1) samples

accumulate ac [] = []
accumulate ac (x:xs) = ac + x : accumulate (ac + x) xs
safe total = if total /= 0 then total else 1
makeLine total = bar . (`div` safe total) . (* height)

getCpuData = readFile "/proc/stat" >>= return . map(read) . words . head . lines

readKeyValueFile pp filename = readFile filename >>= return . makeMap where
  makeMap l = fromList $ map parseLine . lines $ l
  parseLine l = (strip k, pp . words $ v) where
     (k,v) = split1 ':' l

readBatteryFile = readKeyValueFile head
readNetFile = readKeyValueFile $ map read

newtype IOBox = IOBox { exec :: IO (String, IOBox) }
data Geometry = L Int | R Int
type BoxIO = IO (String, IOBox)
type BoxState = StateT BoxIO IO
type Graph = [[String]]

staticMessage :: String -> Int -> BoxIO
staticMessage x size = do
  return (x, IOBox { exec = staticMessage x size })

emptySpace = staticMessage ""
exit _ = print "end of input" >> getProcessGroupID >>= signalProcessGroup softwareTermination >> return ""
genTitle w = getLine `catch` exit >>= replaceIcon >>= \x -> return (x, IOBox { exec = genTitle w })

frame2 cmd width offset = printf fmt graphBackgroundColor cmd width height offset where
  fmt = "^ib(1)^fg(%s)^ca(1,%s)^r(%dx%d)^ca()^p(%d)^fg()"

showGraph :: [String] -> String -> Graph -> String
showGraph colorTable cmd state = frame' ++ bars where
  frame' = frame2 cmd len (0::Int)
  bars = concat . map showOneColor $ zip colorTable state
  showOneColor (col, ar) = move(-len) ++ (color col $ concat $ ar)
  len = case state of (x:xs) -> length x

clock width = do
  time <- liftIO getCurrentTime
  timezone <- liftIO getCurrentTimeZone
  let tod = localTimeOfDay $ utcToLocalTime timezone time
  let (h,m) = (todHour tod, todMin tod)
  let clockDisplay = color "#C7AE86" $ printf "%02d:%02d" h m
  let frame' = frame2 "clock.sh" width $ -width + padding
  return (frame' ++ clockDisplay, IOBox { exec = clock width })

mem width = do
  let zeroGraph = take 4 $ repeat $ take width $ repeat $ bar 0
  mem' width zeroGraph where
    mem' width graph = do
      memState <- liftIO $ readBatteryFile "/proc/meminfo"
      let newGraph = updateGraph graph $ makeMemSample memState
      return ((showGraph memColorTable "top.sh" newGraph), IOBox { exec = mem' width newGraph})

makeMemSample input = map (makeLine total) values where
  total:free:cached:active:[] = map (read . (input !)) ["MemTotal", "MemFree", "Cached", "Active"]
  values = [total - free, total - free - cached, active]

net dev width = do
  let zeroGraph = take 3 $ repeat $ take width $ repeat $ bar 0
  netState <- liftIO $ readNetFile "/proc/net/dev"
  net' dev width zeroGraph netState where
    net' dev width graph netState = do
      newNetState <- liftIO $ readNetFile "/proc/net/dev"
      let netDelta = delta (newNetState ! dev) (netState ! dev)
      let newGraph = updateGraph graph $ makeNetSample dev netDelta
      return ((showGraph netColorTable "net.sh" newGraph), IOBox { exec = net' dev width newGraph newNetState})

makeNetSample dev input = map (makeLine total) values where
  inbound = log $ (fromIntegral $ input !! 0) / netSilenceThreshold + 1
  outbound = log $ (fromIntegral $ input !! 8) / netSilenceThreshold + 1
  total' = max 22 (inbound + outbound)
  total = truncate total'
  values = map truncate [total', total' - outbound, inbound] :: [Int]

delta newar ar = map (\(n,o) -> n-o) $ zip newar ar

cpu width = do
  let zeroGraph = take 3 $ repeat $ take width $ repeat $ bar 0
  procData <- getCpuData
  cpu' width zeroGraph procData where
    cpu' width graph procData = do
      newProcData <- liftIO $ getCpuData
      let procDelta = delta newProcData procData
      let newGraph = updateGraph graph $ makeCpuSample procDelta
      return ((showGraph cpuColorTable "top.sh" newGraph), IOBox { exec = cpu' width newGraph newProcData })

makeCpuSample :: [Int] -> [String]
makeCpuSample (_ :user:nice:sys:idle:io:tail) = map (makeLine total) values where
  (total:values) = reverse $ accumulate 0 [sys + io, nice, user, idle]


batteryGraph width = do
  batteryInfo <- readBatteryFile "/proc/acpi/battery/BAT0/info"
  let zeroGraph = (take width $ repeat $ bar 0) : []
  batGr' width zeroGraph $ read $ batteryInfo ! "design capacity" where
    batGr' width graph capacity = do
        batteryState <- liftIO $ readBatteryFile "/proc/acpi/battery/BAT0/state"
        let remainingCapacity = read $ batteryState ! "remaining capacity"
        let newGraph = updateGraph graph $ [makeLine capacity remainingCapacity]
        return ((showGraph batteryColorTable "top.sh" newGraph), IOBox { exec = batGr' width newGraph capacity })

battery width = do
  battery' width where
    battery' width = do
        batteryInfo <- readBatteryFile "/proc/acpi/battery/BAT0/info"
        let capacity = read $ batteryInfo ! "design capacity"
        batteryState <- liftIO $ readBatteryFile "/proc/acpi/battery/BAT0/state"
        let batteryFrame = frame2 "powertop.sh" width $ -width + padding
        let rate = read $ batteryState ! "present rate" :: Int
        let remainingCapacity = read $ batteryState ! "remaining capacity"
        let (h, m) = (remainingCapacity * 60 `div` rate) `divMod` 60
        let percent = remainingCapacity * 100 `div` capacity
        let info = case batteryState ! "charging state" of
              "discharging" | rate /= 0 -> printf "%d%%(%02d:%02d)" percent h m
              otherwise -> printf "%d%%C" percent
        return ((batteryFrame ++ color "#C7AE86" info), IOBox { exec = battery' width})


sec = 1000000
never = 100000000 * sec

sendLayout chan n pos msg = writeChan chan (n, printf "^pa(%d)^p()%s" pos msg)

runLayout chan n refresh pos state = do
  (msg, state2) <- exec state
  sendLayout chan n pos msg
  threadDelay $ refresh * sec
  runLayout chan n refresh pos state2

initOneLayout chan pos n func refresh size = do
  (msg, state) <- func size
  sendLayout chan n pos msg
  runLayout chan n refresh pos state

initLayoutAll :: Chan (Int, String) -> Int -> Int
                 -> [(Int, (Int -> BoxIO, Int, Geometry))]
                 -> IO (Int, Int)
initLayoutAll chan offsetL offsetR [] = return (offsetL, offsetR)
initLayoutAll chan offsetL offsetR ((n, (func, refresh, loc)) : xs) =
  case loc of
    L size -> do
       forkIO $ initOneLayout chan offsetL n func refresh size
       initLayoutAll chan (offsetL + size + padding) offsetR xs
    R size -> do
       (offsetL2, offsetR2) <- initLayoutAll chan offsetL offsetR xs
       let offsetR3 = offsetR2 - size - padding
       forkIO $ initOneLayout chan offsetR3 n func refresh size
       return (offsetL2, offsetR3)

mergeTitle :: Chan (Int, String) -> StateT [String] IO ()
mergeTitle chan = forever $ do
  title <- get
  (n, val) <- liftIO $ readChan chan
  let newTitle = l ++ (val : r) where (l, _:r) = splitAt n title
  liftIO $ putStrLn $ concat newTitle
  liftIO $ hFlush stdout
  put newTitle

spawnTrayer xpos = do
  system $ "trayer --expand false --edge top --align right " ++
                    "--widthtype request --height 22 --margin " ++ (show xpos)
  return ()

main = do
  -- fd <- openFd "/tmp/log3.txt" WriteOnly Nothing defaultFileFlags
  -- dupTo fd stdError
  hSetBuffering stdin LineBuffering
  chan <- newChan :: IO (Chan (Int, String))
  screenWidth <- getScreenWidth
  (_, offsetR) <- initLayoutAll chan 0 (screenWidth + padding `div` 2) $ zip (enumFrom 0) layout
  forkProcess $ spawnTrayer $ screenWidth - offsetR + padding `div` 2
  let emptyTitle = take (length layout) . repeat $ ""
  evalStateT (mergeTitle chan) emptyTitle

getWinId s = (l, w, r) where
  (l,xs) = split1 '{' s
  (w,r) = split1 '}' xs

replaceIcon title = do
  let (l,winid,r) = getWinId title
  (_,iconRaw,_) <- readProcessWithExitCode "geticon" [winid] ""
  let iconXpm = formatXPM . scaleRawImage 22 $ iconRaw
  let iconName = printf "icons/i%d.xpm" . abs .hashString $ iconRaw
  exist <- doesFileExist iconName
  if not exist then writeFile iconName iconXpm else return ()
  return $ l ++ "^i(" ++ iconName ++ ") " ++ r
