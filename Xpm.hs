{-# LANGUAGE ForeignFunctionInterface #-}

module Xpm (
  formatXPM,
  scaleRawImage,
  downscaleRawImage,
  scale,
  getIcon,
  getIcons,
  Bitmap,
  getXpmIcon,
  chunksOf
  ) where

import IO
import Text.Printf

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Numeric
import System.Posix.DynamicLinker
import Foreign.C.String
import GHC.Ptr
import Foreign.Ptr
import Foreign.C
import GHC.Word
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek)
foreign import ccall "dynamic" openDisplay__ :: FunPtr XOpenDisplayFunc -> XOpenDisplayFunc
foreign import ccall "dynamic" closeDisplay__ :: FunPtr XCloseDisplayFunc -> XCloseDisplayFunc
foreign import ccall "dynamic" setErrorHandler__ :: FunPtr XSetErrorHandlerFunc -> XSetErrorHandlerFunc
foreign import ccall "dynamic" internAtom__ :: FunPtr XInternIconFunc -> XInternIconFunc
foreign import ccall "dynamic" getWinProp__ :: FunPtr XGetWindowPropertyFunc -> XGetWindowPropertyFunc
foreign import ccall "dynamic" free__ :: FunPtr XFreeFunc -> XFreeFunc

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

strBlend :: String -> (String, String) -> String
strBlend as (cs,bs) = let [a,c,b] = map (fst . head . readHex) [as,cs,bs];
                          res = (c * a + b * (255 - a)) `div` 255 :: Int in
                   printf "%02X" res

convertColor :: String -> String
convertColor c =
  let rgba = zip (chunksOf 2 c) (chunksOf 2 $ tail bgColor ++ "00");
      ((a,_) : bgr) = reverse rgba in
  concat . map (strBlend a) . reverse $ bgr


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

type XOpenDisplayFunc = CString -> IO (Ptr Int)
type XInternIconFunc = Ptr Int -> CString -> CInt -> IO CInt
type XGetWindowPropertyFunc =
     Ptr Int  -- Display* (dpy)
     -> CInt -- Window w (argument)
     -> CInt -- Atom property (atom)
     -> CLLong -- long long offset (0)
     -> CLLong  -- long long_length (1000000)
     -> CInt    -- Bool delete (False = 0)
     -> CLong   -- Atom req_type (AnyPropertyType = 0L)
     -> Ptr CInt  -- Atom *actual_type_return
     -> Ptr CInt  -- int *actual_format_return
     -> Ptr CLong -- unsigned long *nitems_return
     -> Ptr CLong -- unsigned long *bytes_after_return
     -> Ptr (Ptr Word8) -- unsigned char **prop_return
     -> IO CInt
type  XFreeFunc = Ptr Word8 -> IO ()
type  XCloseFunc = Ptr Int -> IO ()
type  XSetErrorHandlerFunc = FunPtr XCloseDisplayFunc -> IO (FunPtr XCloseDisplayFunc)
type  XCloseDisplayFunc = Ptr Int ->IO ()

getIconProperty win = withDL "libX11.so" [RTLD_NOW] $ \mod -> do
   openDisplayPtr <- dlsym mod "XOpenDisplay"
   closeDisplayPtr <- dlsym mod "XCloseDisplay"
   setErrorHandlerPtr <- dlsym mod "XCloseDisplay"
   internAtomPtr <- dlsym mod "XInternAtom"
   getWinPropPtr <- dlsym mod "XGetWindowProperty"
   freePtr <- dlsym mod "XFree"
   let xOpenDisplay = openDisplay__ openDisplayPtr
       xCloseDisplay = closeDisplay__ closeDisplayPtr
       xSetErrorHandler = setErrorHandler__ setErrorHandlerPtr
       xInternAtom = internAtom__ internAtomPtr
       xGetWindowProperty = getWinProp__ getWinPropPtr
       xFree = free__ freePtr
   dpy <- withCString ":0" $ xOpenDisplay
   --xSetErrorHandler closeDisplayPtr
   atom <- withCString "_NET_WM_ICON" $ \iconAtomName -> xInternAtom dpy iconAtomName 0
   -- print dpy
   -- print atom
   alloca $ \actualTypeReturn ->
     alloca $ \actualFormatReturn ->
       alloca $ \nitemsReturn ->
         alloca $ \bytesAfterReturn ->
           alloca $ \propReturn -> do
             res <- xGetWindowProperty dpy win atom 0 1000000 0 0 actualTypeReturn
                                       actualFormatReturn nitemsReturn bytesAfterReturn
                                       propReturn
             if res /= 0 then return . take 16 . repeat $ 0 else do
               nitems <- peek nitemsReturn
               let nbytes = fromIntegral $ nitems * 8
               propPtr <- peek propReturn
               prop <- peekArray nbytes propPtr
               xFree propPtr
               xCloseDisplay dpy
               return prop

data Bitmap a = Bitmap { width :: Int, height :: Int, pixels :: [a] } deriving (Show)

bgraToRgba [b,g,r,a] = [r,g,b,a]

decodeSize = foldl (\x y -> x * 256 + y) 0 . reverse . map fromIntegral
makeIconList [] = []
makeIconList (w:h:rest) = Bitmap width height (map bgraToRgba pixels) : makeIconList rest' where
  [width,height] = map (decodeSize) $ [w,h]
  (pixels,rest') = splitAt (width * height) rest

bestMatch sz (icon:icons) = bestMatch' sz icon icons where
  bestMatch' sz icon [] = icon
  bestMatch' sz y (x:xs) = bestMatch' sz betterIcon xs where
    betterIcon = if xBetter then x else y
    [wx, wy] = map width [x,y]
    (xBigger, xBigEnough, yBigEnough) = (wx > wy, wx > sz, wy > sz)
    xBetter = (xBigger && not yBigEnough) || (yBigEnough && xBigEnough && not xBigger)

-- TODO: check that neither color nor bg swaped
blend a1 a2 (v1, v2) = fromIntegral $ (a1' * v1' + a2' * v2') `div` 255 where
 [a1', a2', v1', v2'] = map fromIntegral [a1, a2, v1, v2]

setBackground bg color = map (blend (255-a) a) . zip bg $ reverse bgr where
  (a:bgr) = reverse color

setImageBackground txtBg (Bitmap w h px) = Bitmap w h newpx where
  bg = map (fst . head . readHex) . chunksOf 2 $ tail txtBg
  newpx = map (setBackground bg) px

getIcons win = getIconProperty win >>= return . makeIconList
               .  map (take 4) . chunksOf 8

getIcon sz win = do
  icons <- getIcons win
  return . bestMatch sz $ icons
  -- return . setImageBackground bgColor . bestMatch sz $ icons

toTxtColors :: Bitmap [Word8] -> String
toTxtColors (Bitmap w h px) = concat . conv $ px where
  conv [] = []
  conv (x:xs) = (concat . map (printf "%02X") $ x) : conv xs

getXpmIcon sz win = do
  i <- getIcon sz win
  return . formatXPM . scaleRawImage sz . toTxtColors $ i
