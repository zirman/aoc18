module Lib where

import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Maybe as Maybe

someFunc :: IO ()
someFunc = return ()

runOnFile :: Show a => (String -> a) -> String -> IO ()
runOnFile f fname =
    IO.openFile fname IO.ReadMode >>= \handle ->
    IO.hGetContents handle >>= \contents ->
    print (f contents) *>
    IO.hClose handle

toNum :: String -> [Integer]
toNum = fmap f . words
    where f ('+':xs) = read xs
          f ('-':xs) = -(read xs)

day1p1 :: IO ()
day1p1 = runOnFile (sum . toNum) "day1.txt"

day1p2 :: IO ()
day1p2 = runOnFile day1 "day1.txt" where
    day1 = fst . head . filter seenFreq . buildFreqs . cycle . toNum
    buildFreqs xs =
        let freqs = drop 1 $ scanl (+) 0 xs
        in zip freqs $ scanl (flip (:)) [] freqs
    seenFreq (freq, seen) = freq `elem` seen

day2p1 :: IO ()
day2p1 = runOnFile day2 "day2.txt" where
    day2 = product . countRepeats . concatMap countLetters . lines
    countLetters :: String -> [Int]
    countLetters = fmap head . List.group . List.sort . countRepeats
    countRepeats :: Ord a => Eq a => [a] -> [Int]
    countRepeats = filter (/=1) . fmap length . List.group . List.sort

day2p2 :: IO ()
day2p2 = runOnFile day2 "day2.txt" where
    day2 =
        head . head .
        fmap (\[xs,ys]-> findInLists xs ys) .
        filter (\xs -> 2 == length xs) .
        List.groupBy subStrCmp . List.sort .
        fmap subStrs . lines
    subStrs :: [a] -> [[a]]
    subStrs = g [] where
        g ps [] = []
        g ps (x : ts) = (ps ++ ts) : g (ps ++ [x]) ts
    subStrCmp :: Eq a => [[a]] -> [[a]] -> Bool
    subStrCmp xs ys = any (`elem` ys) xs
    findInLists :: (Foldable t1, Foldable t2, Eq b) => t2 b -> t1 b -> [b]
    findInLists xs = concatMap (\y -> Maybe.maybeToList (List.find (== y) xs))
