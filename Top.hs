
module Top (
  makeCpuMap,
  makeCpuDiff
  ) where


import Directory
import Data.Char
import Text.Printf
import qualified Data.Map as M
import Control.Concurrent
import Data.Function
import Data.List
import Utils

cpuStat file = (pid, usage) where
  w = words file
  pid = head w
  usage = sum . map read . take 3 . drop 13 $ w

perSec :: Double -> Integer -> Integer
perSec sec val = truncate $ (fi val) / sec

makeCpuDiff newCpuInfo cpuInfo sec = do
  let diff = M.differenceWith (\a b -> Just $ a - b) newCpuInfo cpuInfo
  let active = M.filter ( /= 0 ) diff
  let sorted = take 3 . reverse . sortBy (compare `on` snd) $ M.toList active
  files <- mapM (readFile . printf "/proc/%s/stat" . fst) sorted
  let names = map ((!!1) . words) files :: [String]
  return $ map (pair $ printf "   %2d%% - %s") $ zip (map ((perSec sec) . snd) sorted) names :: IO [String]

makeCpuMap :: IO (M.Map String Integer)
makeCpuMap = do
  x <- getDirectoryContents "/proc"
  let pids = map (printf "/proc/%s/stat") $ filter (isDigit . head) $ x :: [String]
  files <- mapM readFile pids
  return $! M.fromList $ map cpuStat files

main = do
  cpuInfo <- makeCpuMap
  threadDelay 1000000
  newCpuInfo <- makeCpuMap
  makeCpuDiff newCpuInfo cpuInfo 1
  threadDelay 1000000
  newCpuInfo <- makeCpuMap
  makeCpuDiff newCpuInfo cpuInfo 2
