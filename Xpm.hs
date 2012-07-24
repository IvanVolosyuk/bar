
module Xpm (
  formatXPM,
  scaleRawImage,
  downscaleRawImage,
  scale
  ) where

import IO
import Text.Printf

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Numeric

bgColor = "#BEBEBE"

xpm0 = "/* XPM */"
xpm1 = "static char *konsole[] = {"
xpm2 = "/* columns rows colors chars-per-pixel */"
xpm3 w colors charPerPixel = printf "\"%d %d %d %d\"," w w colors charPerPixel
xpm4 = "/* pixels */"
symbols = " .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjklzxcvbnmMNBVCZASDFGHJKLPIUYTREWQ!~^/()_`'][{}|"

chunksOf n [] = []
chunksOf n s = x : chunksOf n xs where (x,xs) = splitAt n s

colorGen n prefix = case n of
  1 -> map (\x -> x : prefix) symbols
  otherwise -> concat $ map (\x -> colorGen (n-1) (x : prefix)) symbols 
 

formatColor :: (String,String) -> String
formatColor (col,ch) = printf "\"%s c #%s\"" ch newcol where newcol = convertColor col

join sep [] = ""
join sep (x:xs) = foldl (\x y ->  x++sep++y ) x xs

blend :: String -> (String, String) -> String
blend as (cs,bs) = let [a,c,b] = map (fst . head . readHex) [as,cs,bs] in
                   let res = (c * a + b * (255 - a)) `div` 255 :: Int in
                   printf "%02X" res
 
convertColor :: String -> String
convertColor c =
  let rgba = zip (chunksOf 2 c) (chunksOf 2 $ tail bgColor ++ "00") in
  let ((a,_) : bgr) = reverse rgba in
  concat . map (blend a) . reverse $ bgr


imageWidth text = truncate . sqrt $ (fromIntegral . length $ text) / 8
 
formatXPM :: String -> String
formatXPM text = unlines [ xpm0, xpm1, xpm2, meta, colors, picture, "};" ] where
  width = imageWidth text
  colorSet = S.fromList $ chunksOf 8 text
  charsPerPixel = if (S.size colorSet) > (length symbols) then 2 else 1 :: Int
  colorMapList = zip (S.toList colorSet) (colorGen charsPerPixel "")
  meta = xpm3 width (length colorMapList) charsPerPixel
  colorMapStrings = map formatColor colorMapList
  colors = join ",\n" $ colorMapStrings ++ [ xpm4 ]
  colorMap = M.fromList colorMapList
  textConv = concat . map (colorMap !) $ chunksOf 8 text
  picture = join ",\n" $ map (printf "\"%s\"") $ chunksOf (width * charsPerPixel) textConv

scale :: Int -> [a] -> [a]
scale newsize ar = pick newsize newsize size ar where
  size = length ar
  pick acc newsize size [] = []
  pick acc newsize size (x:xs) =
    if acc >= size
    then (x : pick (acc - size) newsize size (x:xs))
    else pick (acc + newsize) newsize size xs


downscaleRawImage :: Int -> String -> String
downscaleRawImage sz text =
  let width = imageWidth text in
  if width > sz
  then scaleRawImage sz text
  else text

scaleRawImage :: Int -> String -> String
scaleRawImage sz text = newtext where
  width = imageWidth text
  newtext = concat . scale sz . (map scaleLine) . chunksOf (8 * width) $ text
  scaleLine = concat . scale sz . (chunksOf 8)
