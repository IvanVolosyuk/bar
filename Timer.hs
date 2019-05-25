{-# LANGUAGE Arrows #-}
module Timer (
  timerTask,
  getRootTickEvents,
  Period,
  Timestamp,
  Size(..),
  RootChan,
  RootInput(..),
  TimerCollectionInp(..),
  epoch,
  timerMain
  ) where

import Control.Applicative
import Control.Auto
import Control.Auto.Blip
import Control.Auto.Blip.Internal
import Control.Auto.Collection
import Control.Auto.Core
import Control.Concurrent
import Control.Monad
import Control.Monad.Loops
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe
import Data.Time
import Data.Time.Clock.POSIX
import Data.Traversable
import Text.Printf
import Prelude hiding ((.), id)

import qualified Data.Map as M

import Icon
import Graphics.X11.Xlib

data Size = Size {x_ :: Int, y_ :: Int} deriving (Show, Eq)
data RootInput = RNop | RTick | RTitle String | RExpose Window 
                      | RMotion Window (Maybe Size)
                      | RClick Window Size deriving Show
type RootChan = Chan RootInput

type Period = NominalDiffTime
type Timestamp = NominalDiffTime


data TimerInp = TNop | TDel | TTick Timestamp deriving Show
data TimerCollectionInp = TCNop
                        | TCChangeUsage [(Period,Int)]
                        | TCTick Timestamp
                        deriving Show

type TimerAuto = Interval IO TimerInp (Blip Timestamp)

epoch = posixSecondsToUTCTime 0

getRootTickEvents :: RootInput -> IO (Maybe TimerCollectionInp)
getRootTickEvents  RTick = Just . TCTick . (`diffUTCTime` epoch) <$> getCurrentTime
getRootTickEvents _      = return Nothing

getTimerChangeEvents :: TimerCollectionInp -> Maybe [(Period, Int)]
getTimerChangeEvents (TCChangeUsage chg) = Just chg
getTimerChangeEvents _                   = Nothing

getTimerTickEvents :: (M.Map Period Int, TimerCollectionInp) -> M.Map Period TimerInp
getTimerTickEvents (mp, evt) =
  case evt of
     TCTick ts       -> fmap (conv $ TTick ts) mp
     TCChangeUsage _ -> fmap (conv TNop) mp
     _               -> M.empty
  where
     conv dflt 0         = TDel
     conv dflt u | u > 0 = dflt
     conv _ _            = error "Negative usage"

timerCollection :: RootChan -> Auto IO (M.Map Period TimerInp) (M.Map Period (Blip Timestamp))
timerCollection ch = muxManyI_ (timerInit ch)

timerTask :: RootChan -> Auto IO TimerCollectionInp [(Period,UTCTime)]
timerTask ch = proc inpEvt -> do
    usages <- scanB_ updateUsages M.empty <<< emitJusts getTimerChangeEvents -< inpEvt
    timersTs <- timerCollection ch -< getTimerTickEvents (usages, inpEvt)
    let timestamps = filterTs timersTs
    id -< M.toList timestamps
  where
    updateUsages mp updates = M.unionWith (+) mp (M.fromList updates)
    filterTs = fmap ((`addUTCTime` epoch) . blip 0 id) . M.filter triggered
    triggered (Blip x) = True
    triggered NoBlip = False


timerInit :: RootChan -> Period -> TimerAuto 
timerInit rootCh period = mkAutoM_ $ \inp -> do 
  print ("timerInit", period, inp)
  case inp of
    TDel -> return (Nothing, timerInit rootCh period)
    _    -> do
      delayCh <- newChan 
      thr <- forkIO $ forever $ do
        targetTs <- (`addUTCTime` epoch) <$> readChan delayCh
        dt <- diffUTCTime targetTs <$> getCurrentTime
        when (dt > 0) $ threadDelay (round $ dt * 1000000)
        writeChan rootCh RTick
      writeChan delayCh 0 -- wake up asap
      return (Just NoBlip, tickTimer thr delayCh rootCh Nothing period)

-- Produces next timestamp for given timestamp and period
tickTimer :: ThreadId -> Chan NominalDiffTime -> RootChan -> Maybe Timestamp -> Period -> TimerAuto
tickTimer thr delayCh rootCh start period = mkAutoM_ $ \inp -> do
  let timeToNextTick ts = period - snd (properFraction (ts/period)) * period
  -- print ("tickTimer", start, period, inp)

  case inp of
    TNop -> return (Just NoBlip, tickTimer thr delayCh rootCh start period)
    TDel -> do
      print ("Kill thread", period)
      killThread thr
      return (Nothing, timerInit rootCh period)
    TTick tick -> case start of
         Nothing -> do
           let ts' = tick + timeToNextTick tick
           -- print ("Next wakeup ", tick, ts')
           writeChan delayCh ts'
           return (Just (Blip tick), tickTimer thr delayCh rootCh (Just ts') period)
         Just start' ->
           if tick > start'
           then do
             -- print ("Next periodic ", start' + period)
             writeChan delayCh (start' + period)
             return (Just (Blip start'), tickTimer thr delayCh rootCh (Just $ start' + period) period)
           else return (Just NoBlip, tickTimer thr delayCh rootCh start period)


timerMain = do
  ch <- newChan :: IO RootChan
  forkIO $ forever $ do
    line <- getLine
    writeChan ch (RTitle line)
  {- stress test
  forkIO $ forever $ do
    threadDelay 10000
    writeChan ch (RTitle "c 0.0001")
    threadDelay 10000
    writeChan ch (RTitle "d 0.0001")
  -}

  epoch <- parseTimeM True defaultTimeLocale "%Y" "2019" :: IO UTCTime
  startTs <- (`diffUTCTime` epoch)<$> getCurrentTime
  let t = timerTask ch
  let sec = fromRational 1 :: NominalDiffTime
  let makeDt dt = fromIntegral (round $ dt * 1000000) * sec / 1000000

  (v2, t2) <- stepAuto t $ TCChangeUsage [(5, 1), (3, 1)]
  print v2
  (v3, t3) <- stepAuto t2 $ TCTick startTs
  print v3
  let run t' = do
      -- print "Waiting for cmds"
      cmd <- readChan ch
      case cmd of
        RTick -> do
          ts <- (`diffUTCTime` epoch)<$> getCurrentTime
          (v'', t'') <- stepAuto t' (TCTick ts)
          print v''
          run t''
        RTitle msg ->
          case words msg of
            ("c":n:_) -> do
              let period = read n :: Double
              (v'', t'') <- stepAuto t' (TCChangeUsage [(makeDt period, 1)])
              print v''
              run t''
            ("d":n:_) -> do
              let period = read n :: Double
              (v'', t'') <- stepAuto t' (TCChangeUsage [(makeDt period, -1)])
              print v''
              run t''
            cmd -> print ("Unknown cmd", cmd) >> run t'

        _ -> do
          print ("Unknown cmd: ", cmd)
          run t'
  run t3
           

  let input = [
               TCTick 123.5,
               TCTick 123.6,
               TCTick 123.7,
               TCTick 124.5,
               TCTick 125.5,
               TCTick 126.5,
               TCChangeUsage [(10, 1), (2, -1)],
               TCChangeUsage [(10, 1), (2, 0)],
               TCTick 127.5,
               TCTick 128.5
              ]

  out <- streamAuto t input
  mapM_ print $ zip input out
  return ()
