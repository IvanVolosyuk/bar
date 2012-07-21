
module Xpm (
  formatXPM
  ) where

import IO
import Text.Printf

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Numeric
import Data.Char

bgColor = "#BEBEBE"

xpm0 = "/* XPM */"
xpm1 = "static char *konsole[] = {" 
xpm2 = "/* columns rows colors chars-per-pixel */"
xpm3 w colors charPerPixel = printf "\"%d %d %d %d\"," w w colors charPerPixel
xpm4 = "/* pixels */"
chars = " .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjklzxcvbnmMNBVCZASDFGHJKLPIUYTREWQ!~^/()_`'][{}|"
nchars = length chars

split n [] = []
split n s = x : split n xs where (x,xs) = splitAt n s

colorGen n prefix = case n of
  1 -> map (\x -> x : prefix) chars
  otherwise -> concat $ map (\x -> colorGen (n-1) (x : prefix)) chars 
 

formatColor :: (String,String) -> String
formatColor (col,ch) = printf "\"%s c #%s\"" ch newcol where newcol = convertColor col

join sep [] = ""
join sep (x:xs) = foldl (\x y ->  x++sep++y ) x xs

blend :: String -> (String, String) -> String
blend as (cs,bs) = let [a,c,b] = map (fst . head . readHex) [as,cs,bs] in
                   let res = (c * a + b * (255 - a)) `div` 255 :: Int in
                   map toUpper $ printf "%02X" $ snd $ res `divMod` 256
 
convertColor :: String -> String
convertColor c = 
  let rgba = zip (split 2 c) (split 2 $ tail bgColor ++ "00") in
  let ((a,_) : bgr) = reverse rgba in
  concat . map (blend a) . reverse $ bgr
 
formatXPM :: String -> String
formatXPM text = unlines [ xpm0, xpm1, xpm2, meta, colors, picture, "};" ] where
  meta = xpm3 width (length colorMapList) charsPerPixel
  colors = join ",\n" $ colorMapStrings ++ [ xpm4 ]
  picture = join ",\n" $ map (printf "\"%s\"") $ split (width * charsPerPixel) textConv
  width = truncate . sqrt $ (fromIntegral . length $ text) / 8 :: Int
  colorMapList = zip (S.toList colorSet) (colorGen charsPerPixel "")
  charsPerPixel = if (S.size colorSet) > nchars then 2 else 1 :: Int
  colorMapStrings = map formatColor colorMapList
  colorSet = S.fromList $ split 8 text
  textConv = concat . map (colorMap !) $ split 8 text
  colorMap = M.fromList colorMapList

main = readFile "icon.raw" >>= putStr . formatXPM

