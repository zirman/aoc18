module Lib where

import System.IO

someFunc :: IO ()
someFunc = return ()

runOnFile :: (String -> IO a) -> String -> IO ()
runOnFile f fname =
    openFile fname ReadMode >>= \handle ->
    hGetContents handle >>= \contents ->
    f contents *>
    hClose handle

toNum :: String -> [Integer]
toNum = fmap f . words
    where f ('+':xs) = read xs
          f ('-':xs) = -(read xs)

day1p1 :: IO ()
day1p1 = runOnFile (print . sum . toNum) "day1.txt"

day1p2 :: IO ()
day1p2 = runOnFile (print . fst . head . filter seenFreq . buildFreqs . cycle . toNum) "day1.txt"
    where buildFreqs xs = let freqs = drop 1 $ scanl (+) 0 xs
                          in zip freqs $ scanl (flip (:)) [] freqs
          seenFreq (freq, seen) = freq `elem` seen
