
module Utils (
  pair,
  join,
  split,
  split1,
  strip,
  fi,
  loop
  ) where

pair op (a,b) = op a b
join sep [] = ""
join sep (x:xs) = foldl (\x y ->  x++sep++y ) x xs

split :: Char -> String -> [String]
split ch s =  case dropWhile (==ch) s of
  "" -> []
  s' -> word : split ch s''
    where (word, s'') = break (==ch) s'

strip :: String -> String
strip s = reverse . dropWhile p . reverse . dropWhile p $ s where
  p = (==' ')

split1 ch s = (x, safeTail xs) where
  safeTail [] = []
  safeTail (x:xs) = xs
  (x,xs) = break (==ch) s

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

loop f input = do
  output <- f input
  loop f output

