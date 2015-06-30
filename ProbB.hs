module Main (main) where

import Control.Monad (replicateM, when)

input :: IO ()
input = do
    n <- readLn
    when (n /= 0) $ do
        w <- replicateM n getLine
        putStrLn $ show $ solve $ zip [1..] w
        input

solve :: [(Int, String)] -> Int
solve w = count w w wlen 0
  where
    wlen = [5, 7, 5, 7, 7]
    count _ ((j, _):_) [] _ = j
    count _ [] _ _ = 0
    count [] _ _ _ = 0
    count ((_, x):xs) a@((_, y):ys) b@(z:zs) n
        | n + length x == z = count xs a zs 0
        | n + length x > z = count ys ys wlen 0
        | otherwise = count xs a b (n + length x)

main :: IO ()
main = input

