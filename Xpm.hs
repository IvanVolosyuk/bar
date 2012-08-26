{-# LANGUAGE ForeignFunctionInterface #-}

module Xpm (
  formatXPM,
  Bitmap,
  chunksOf,
  getIconPath,
  IconState,
  initState,
  defaultIconConfig,
  IconConfig,
  postProcessing,
  bgColor,
  pickSize,
  cacheIcon,
  scaleNearest,
  scaleLinear,
  Math,
  scale,
  colorFilter,
  black,
  none,
  setColor,
  shift,
  resize,
  shadow,
  scale1D,
  scale2D,
  pair
  ) where

import IO
import Text.Printf

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map ((!))
import Data.Maybe
import Numeric
import Control.Monad.State (StateT, get, put, liftIO, runStateT)
import System.Posix.DynamicLinker
import Foreign.C.String
import GHC.Ptr
import Foreign.Ptr
import Foreign.C
import Foreign (sizeOf)
import GHC.Word
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Storable (peek, peekByteOff)
import Data.HashTable (hashString)
import System.Directory (doesFileExist)
import Utils

foreign import ccall "dynamic" openDisplay__ :: FunPtr XOpenDisplayFunc -> XOpenDisplayFunc
foreign import ccall "dynamic" closeDisplay__ :: FunPtr XCloseDisplayFunc -> XCloseDisplayFunc
foreign import ccall "dynamic" setErrorHandler__ :: FunPtr XSetErrorHandlerFunc -> XSetErrorHandlerFunc
foreign import ccall "dynamic" internAtom__ :: FunPtr XInternIconFunc -> XInternIconFunc
foreign import ccall "dynamic" getWinProp__ :: FunPtr XGetWindowPropertyFunc -> XGetWindowPropertyFunc
foreign import ccall "dynamic" free__ :: FunPtr XFreeFunc -> XFreeFunc
foreign import ccall "wrapper" wrap :: XErrorHandler -> IO (FunPtr XErrorHandler)

background = "#BEBEBE"

xpm0 = "/* XPM */"
xpm1 = "static char *icon[] = {"
xpm2 = "/* columns rows colors chars-per-pixel */"
xpm3 w h colors charPerPixel = printf "\"%d %d %d %d\"," w h colors charPerPixel
xpm4 = "/* pixels */"
symbols = " .XoO+@#$%&*=-;:>,<1234567890qwertyuipasdfghjklzxcvbnmMNBVCZASDFGHJKLPIUYTREWQ!~^/()_`'][{}|"

chunksOf n [] = []
chunksOf n s = x : chunksOf n xs where (x,xs) = splitAt n s

colorGen n prefix = case n of
  1 -> map (\x -> x : prefix) symbols
  otherwise -> concat $ map (\x -> colorGen (n-1) (x : prefix)) symbols


formatColor :: (String,String) -> String
formatColor (col,ch) = printf "\"%s c #%s\"" ch col

formatXPM :: String -> Bitmap [Word8] -> String
formatXPM bg (Bitmap width height px) = image where
  image =  unlines [ xpm0, xpm1, xpm2, meta, colors, picture, "};" ]
  text = toTxtColors . map (setBackground $ toColor bg) $ px
  colorSet = S.fromList $ chunksOf 6 text -- FIXME: repeated 7 lines below
  charsPerPixel = if (S.size colorSet) > (length symbols) then 2 else 1 :: Int
  colorMapList = zip (S.toList colorSet) (colorGen charsPerPixel "")
  meta = xpm3 width height (length colorMapList) charsPerPixel
  colorMapStrings = map formatColor colorMapList
  colors = join ",\n" $ colorMapStrings ++ [ xpm4 ]
  colorMap = M.fromList colorMapList
  textConv = concat . map (colorMap !) $ chunksOf 6 text
  picture = join ",\n" $ map (printf "\"%s\"") $ chunksOf (width * charsPerPixel) textConv

type XOpenDisplayFunc = CString -> IO (Ptr CInt)
type XInternIconFunc = Ptr CInt -> CString -> CInt -> IO CInt
type XGetWindowPropertyFunc =
     Ptr CInt  -- Display* (dpy)
     -> CInt -- Window w (argument)
     -> CInt -- Atom property (atom)
     -> CLong -- long long_offset (0)
     -> CLong  -- long long_length (1000000)
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
type  XSetErrorHandlerFunc = FunPtr XErrorHandler -> IO (FunPtr XErrorHandler)
type  XCloseDisplayFunc = Ptr Int ->IO ()

type XErrorHandler = Ptr CInt -> Ptr CInt -> IO CInt

errorHandler :: XErrorHandler
errorHandler a b = do
  print "X11 Error!"
  let res = 0 :: CInt
  return res

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

blend a1 a2 (v1, v2) = fromIntegral $ (a1' * v1' + a2' * v2') `div` 255 where
 [a1', a2', v1', v2'] = map fromIntegral [a1, a2, v1, v2]

setBackground bg color = map (blend (255-a) a) . zip bg $ reverse bgr where
  (a:bgr) = reverse color

onTop bg color = map (blend (255-a) a) $ zip bg color where
  a = last color

toColor str = map (fst . head . readHex) . chunksOf 2 $ tail str 

toTxtColors :: [[Word8]] -> String
toTxtColors = concat . map (concat . map (printf "%02X"))

data IconConfig = IconConfig { pickSize :: Int
                             , postProcessing :: Bitmap [Word8] -> Bitmap [Word8]
                             , bgColor :: String
                             , cacheIcon :: Bool
                             }

data IconState = IconState { propFunc :: XGetWindowPropertyFunc
                           , freeFunc :: XFreeFunc
                           , dpy :: Ptr CInt
                           , atom :: CInt
                           , iconCache :: M.Map CInt String
                           , iconConfig :: IconConfig
                           }

defaultIconConfig = IconConfig {
   pickSize=16, postProcessing=(scaleNearest 16), bgColor="#000000",
   cacheIcon=True
   }

initState :: IconConfig -> IO IconState
initState iconConfig = do
   mod <- dlopen "libX11.so" [RTLD_NOW]
   openDisplayPtr <- dlsym mod "XOpenDisplay"
   internAtomPtr <- dlsym mod "XInternAtom"
   getWinPropPtr <- dlsym mod "XGetWindowProperty"
   setErrorHandlerPtr <- dlsym mod "XSetErrorHandler"
   freePtr <- dlsym mod "XFree"
   let xOpenDisplay = openDisplay__ openDisplayPtr
       xInternAtom = internAtom__ internAtomPtr
       xGetWindowProperty = getWinProp__ getWinPropPtr
       xSetErrorHandler = setErrorHandler__ setErrorHandlerPtr
       xFree = free__ freePtr
   dpy <- withCString ":0" $ xOpenDisplay
   atom <- withCString "_NET_WM_ICON" $ \atomName -> xInternAtom dpy atomName 0
   wrap errorHandler >>= xSetErrorHandler
   return $ IconState xGetWindowProperty xFree dpy atom M.empty iconConfig

fastHash :: Ptr Word8 -> Int -> IO Int
fastHash p sz = genHash where
  genHash = do
     h <- fastHash' 0 0
     return . snd $ h `divMod` 4000000007 :: IO Int
  fastHash' idx hash = case idx < sz of
    False -> return hash
    True -> do
       b <- peekByteOff p idx :: IO Word8
       fastHash' (idx + 37) (hash * 37 + (fromIntegral b))

makeFileName p sz = do
   h <- fastHash p sz
   return $ printf "icons/i%d.xpm" h :: IO String

ptr_size = sizeOf (undefined :: CLong)

fetchIcon st win =
  alloca $ \actualTypeReturn ->
    alloca $ \actualFormatReturn ->
      alloca $ \nitemsReturn ->
        alloca $ \bytesAfterReturn ->
          alloca $ \propReturn -> do
            res <- (propFunc st) (dpy st) win (atom st) 0 1000000 0 0
                   actualTypeReturn actualFormatReturn nitemsReturn
                   bytesAfterReturn propReturn
            nitems <- peek nitemsReturn
            -- print $ "res:" ++ (show res)
            -- print $ "nitems:" ++ (show nitems)
            if res /= 0 || nitems == 0 then return Nothing else do
              let nbytes = fromIntegral $ nitems * (fromIntegral ptr_size)
              propPtr <- peek propReturn
              return $ Just (propPtr, nbytes)

scaleSimple :: Int -> Int -> [a] -> [a]
scaleSimple size newsize ar = pick newsize newsize size ar where
  pick acc newsize size [] = []
  pick acc newsize size (x:xs) =
    if acc >= size
    then (x : pick (acc - size) newsize size (x:xs))
    else pick (acc + newsize) newsize size xs

class Math a where
  mul :: Int -> a -> a
  add :: a -> a -> a
  normalize :: Int -> a -> a
  scale :: Int -> Int -> [a] -> [a]
  scale size newsize (a:ar) = pick 0 0 0 zero (a:ar) where
    zero = mul 0 a
    pick pos newpos maxpos acc [] = []
    pick pos newpos maxpos acc (a:ar) = if (pos + newsize) < (newpos + size)
      then let pos' = pos + newsize;
               acc' = ((pos' - maxpos) `mul` a) `add` acc in
           pick pos' newpos pos' acc' ar
      else let newpos' = newpos + size;
               acc' = ((newpos' - maxpos) `mul` a) `add` acc in
           (acc' : pick pos newpos' newpos' zero (a:ar))
    

instance Math Int where
  mul a x = a * x
  normalize a x = x `div` a
  add x y = x + y

instance (Math a) => Math [a] where
  mul a xs = map (mul a) $ xs
  normalize a xs = map (normalize a) $ xs
  add xs ys = map (pair add) $ zip xs ys

scale1D newsize a = map (normalize size) . scale size newsize $ a where
  size = length a

scale2D :: Int -> Int -> Int -> [[Int]] -> [[Int]]
scale2D w h newsize = concat . (normalize (w * h)) . scale h newsize
                   . (map $ scale w newsize) . chunksOf w

scaleNearest :: Int -> Bitmap [Word8] -> Bitmap [Word8]
scaleNearest sz (Bitmap w h px ) = Bitmap sz sz . concat 
     . scaleSimple w sz . (map $ scaleSimple w sz) . chunksOf w $ px

scaleLinear sz (Bitmap w h px) = Bitmap sz sz
      . cast . scale2D w h sz . cast $ px

cast :: (Integral a, Num b) => [[a]] -> [[b]]
cast = map (map fromIntegral)

black [r,g,b,a] = [0,0,0,a]
none [r,g,b,a] = [0,0,0,0]
setColor str = \[_,_,_,a2] -> [r,g,b, fromIntegral $ (fromIntegral a2 * fromIntegral a) `div` 255] where
  [r,g,b,a] = toColor str

colorFilter :: ([Word8] -> [Word8]) -> Bitmap [Word8] -> Bitmap [Word8]
colorFilter f (Bitmap w h px) = Bitmap w h newpx where
  newpx = map f $ px

shift :: Int -> Int -> Bitmap [Word8] -> Bitmap [Word8]
shift x y (Bitmap w h px) = Bitmap w h (concat newgrid2) where
  grid = chunksOf w px
  row = map none $ head grid
  cell = none . head $ px
  newgrid = case y > 0 of
    True -> (take y $ repeat $ row) ++ (take (h - y) grid)
    False -> (drop (-y) grid) ++ (take (-y) $ repeat $ row)
  newgrid2 = case x > 0 of
    True -> map (\r -> (take x $ repeat $ cell) ++ (take (w - x) r)) newgrid
    False -> map (\r -> (drop (-x) r) ++ (take (-x) $ repeat $ cell)) newgrid

resize w2 h2 (Bitmap w h px) = Bitmap w2 h2 $ concat newgrid2 where
  grid = chunksOf w px
  row = map none $ head grid
  cell = none . head $ px
  newgrid = (take (min h h2) grid) ++ (take (max 0 $ h2 - h) $ repeat $ row)
  newgrid2 = map (\r -> (take (min w w2) r) ++ (take (max 0 $ w2 - w) $ repeat $ cell)) newgrid

layer :: Bitmap [Word8] -> Bitmap [Word8] -> Bitmap [Word8]
layer (Bitmap w h px) (Bitmap w2 h2 px2) = Bitmap (min w w2) (min h h2) newpx where
  grid = chunksOf w px
  grid2 = chunksOf w2 px2
  newpx = concat . map (map (pair onTop) . pair zip) $ zip grid2 grid

shadow :: Int -> String -> Bitmap [Word8] -> Bitmap [Word8]
shadow off c b = layer b2 $ shift off off . colorFilter (setColor c) $ b2 where
  b2 = resize (off + (width b)) (off + (height b)) b

getIconPath :: CInt -> StateT IconState IO String
getIconPath win = do
  st <- get
  let cfg = iconConfig st
  case M.lookup win (iconCache st) of
    Just path -> return path
    Nothing -> do
      mbIcons <- liftIO $ fetchIcon st win
      case mbIcons of
        Nothing -> return "icons/default.xpm"
        Just (propPtr, nbytes) -> do
          fileName <- liftIO $ makeFileName propPtr nbytes
          let cache = M.insert win fileName (iconCache st)
          put st { iconCache = cache }
          exist <- liftIO $ doesFileExist fileName
          if not exist || not (cacheIcon cfg)
             then do
               prop <- liftIO $ peekArray nbytes propPtr
               let icons = makeIconList . map (take 4) . chunksOf ptr_size $ prop
                   icon = bestMatch (pickSize cfg) icons
                   xpm = formatXPM (bgColor cfg) . (postProcessing cfg) $ icon
               liftIO $ writeFile fileName xpm
             else return ()
          liftIO $ (freeFunc st) propPtr
          return fileName
