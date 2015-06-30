module Main (main) where

import Control.Monad (replicateM, when)
import Data.List (maximumBy)

input :: IO ()
input = do
    [m, nMin, nMax] <- map read <$> words <$> getLine
    when (m /= 0) $ do
        p <- replicateM m readLn
        putStrLn $ show $ solve nMin nMax p
        input

solve :: Int -> Int -> [Int] -> Int
solve nMin nMax p = nMin + (fst $ maximumBy g $ zip [0..] $ f sub)
  where
    sub = take (nMax - nMin + 2) $ drop (nMin - 1) p
    f [x] = [0]
    f (x:xs) = (x - head xs) : f xs
    g (_, x) (_, y) = compare x y

main :: IO ()
main = input

