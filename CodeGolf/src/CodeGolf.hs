module CodeGolf (skips)  where

skips2 :: [a] -> [[a]]
skips2 [] = []
skips2 xs = sk 1 (length xs) xs

sk :: Int -> Int -> [a] -> [[a]]
sk n len xs
 | n > len = []
 | otherwise = fn n xs ++ sk (n+1) len xs

fn :: Int -> [a] -> [[a]]
fn 1 xs = [xs]
fn 2 (x:xs) = [xs]
fn n xs = [[ xs !! (n-1) ]]


skips :: [a] -> [[a]]
skips xs =
  let indexed = zip [1..] xs
      build (i, _) = filter ( \(j,_) -> j `mod` i == 0 ) indexed
      extract pairArray = map snd pairArray  
  in map (extract . build) indexed

  -- snd: Extract the second component of a pair
  -- diference between let...in and where?