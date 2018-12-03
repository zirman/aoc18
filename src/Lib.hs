module Lib where

import qualified System.IO as IO
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Char as Char
import Data.Set (Set)
import qualified Data.Set as Set

someFunc :: IO ()
someFunc = return ()

runOnFile :: Show a => (String -> a) -> String -> IO ()
runOnFile f fname =
    IO.openFile fname IO.ReadMode >>= \handle ->
    IO.hGetContents handle >>= \contents ->
    print (f contents) *>
    IO.hClose handle

toNum :: String -> [Integer]
toNum = fmap f . words where
    f ('+':xs) = read xs
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
    countRepeats = filter (/= 1) . fmap length . List.group . List.sort

day2p2 :: IO ()
day2p2 = runOnFile day2 "day2.txt" where
    day2 =
        head . head .
        fmap (\[xs,ys]-> findInLists xs ys) .
        filter (\xs -> 2 == length xs) .
        List.groupBy subStrCmp . List.sort .
        fmap subStrs . lines
    subStrs :: [a] -> [[a]]
    subStrs = f [] where
        f ps [] = []
        f ps (x : ts) = (ps ++ ts) : f (ps ++ [x]) ts
    subStrCmp :: Eq a => [[a]] -> [[a]] -> Bool
    subStrCmp xs ys = any (`elem` ys) xs
    findInLists :: Foldable t => Eq a => t a -> t a -> [a]
    findInLists xs = concatMap (\y -> Maybe.maybeToList (List.find (== y) xs))

day3p1 :: IO ()
day3p1 = runOnFile day3 "day3.txt" where
    day3 = length . filter (\xs -> length xs > 1) . List.group . List.sort . concatMap (snd . readClaim . words) . lines

readClaim :: [String] -> (String, [(Int, Int)])
readClaim ['#':cNum, "@", offset, size] =
    let (Just i) = List.elemIndex ',' offset
    in let (Just j) = List.elemIndex 'x' size
    in let (x, ',':y) = splitAt i offset
    in let y' = read (filter Char.isDigit y) :: Int
    in let x' = read x :: Int
    in let (w, 'x':h) = splitAt j size
    in let w' = read w :: Int
    in let h' = read h :: Int
    in (cNum, [(a, b) | a <- [x'..(x' + w' - 1)], b <- [y'..y' + h' - 1]])

day3p2 :: IO ()
day3p2 = runOnFile (day3 . claims) "day3.txt" where
    claims :: String -> [(String, [(Int, Int)])]
    claims = fmap (readClaim . words) . lines
    day3 xs = fmap fst . filter (\(cNum, q) -> Set.null (Set.fromList q `Set.intersection` overlaping)) $ xs where
        overlaping :: Set (Int, Int)
        overlaping = Set.fromList (List.concat (filter (\xs -> length xs > 1) . List.group . List.sort . concatMap snd $ xs))
