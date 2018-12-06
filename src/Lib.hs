module Lib where

import System.IO (openFile, hGetContents, hClose, IOMode(..))
import Data.List
    ( find
    , group
    , groupBy
    , sort
    , sortBy
    , minimumBy
    , maximumBy
    , filter
    , elemIndex
    , concat
    )
import Data.Functor (($>))
import Data.Maybe (maybeToList)
import Data.Set (Set, fromList, intersection)
import Data.Char (isDigit, toUpper)

import Data.Time.Calendar (Day(..), fromGregorian)

import Text.Parsec.Prim (ParsecT, Stream)
import Text.ParserCombinators.Parsec
    ( GenParser
    , many
    , many1
    , tokenPrim
    , eof
    , char
    , digit
    , string
    , (<|>)
    , parse
    )

someFunc :: IO ()
someFunc = return ()

runOnFile :: Show a => (String -> a) -> String -> IO ()
runOnFile f fname =
    openFile fname ReadMode >>= \handle ->
    hGetContents handle >>= \contents ->
    print (f contents) *>
    hClose handle

toNum :: String -> [Integer]
toNum = fmap f . words
  where
    f ('+':xs) = read xs
    f ('-':xs) = -(read xs)

day1p1 :: IO ()
day1p1 = runOnFile (sum . toNum) "day1.txt"

day1p2 :: IO ()
day1p2 = runOnFile day1 "day1.txt"
  where
    day1 = fst . head . filter seenFreq . buildFreqs . cycle . toNum
    buildFreqs xs =
        let freqs = drop 1 $ scanl (+) 0 xs
        in zip freqs $ scanl (flip (:)) [] freqs
    seenFreq (freq, seen) = freq `elem` seen

day2p1 :: IO ()
day2p1 = runOnFile day2 "day2.txt"
  where
    day2 = product . countRepeats . concatMap countLetters . lines
    countLetters :: String -> [Int]
    countLetters = fmap head . group . sort . countRepeats
    countRepeats :: Ord a => Eq a => [a] -> [Int]
    countRepeats = filter (/= 1) . fmap length . group . sort

day2p2 :: IO ()
day2p2 = runOnFile day2 "day2.txt"
  where
    day2 =
        head . head .
        fmap (\[xs,ys]-> findInLists xs ys) .
        filter (\xs -> 2 == length xs) .
        groupBy subStrCmp . sort .
        fmap subStrs . lines
    subStrs :: [a] -> [[a]]
    subStrs = f []
      where
        f ps [] = []
        f ps (x:ts) = (ps ++ ts) : f (ps ++ [x]) ts
    subStrCmp :: Eq a => [[a]] -> [[a]] -> Bool
    subStrCmp xs ys = any (`elem` ys) xs
    findInLists :: Foldable t => Eq a => t a -> t a -> [a]
    findInLists xs = concatMap (\y -> maybeToList (find (== y) xs))

day3p1 :: IO ()
day3p1 = runOnFile day3 "day3.txt"
  where
    day3 = length . filter (\xs -> length xs > 1) . group . sort .
        concatMap (snd . readClaim . words) . lines

readClaim :: [String] -> (String, [(Int, Int)])
readClaim ['#':cNum, "@", offset, size] =
    let (Just i) = elemIndex ',' offset
    in let (Just j) = elemIndex 'x' size
    in let (x, ',':y) = splitAt i offset
    in let y' = read (filter isDigit y) :: Int
    in let x' = read x :: Int
    in let (w, 'x':h) = splitAt j size
    in let w' = read w :: Int
    in let h' = read h :: Int
    in (cNum, [(a, b) | a <- [x'..(x' + w' - 1)], b <- [y'..y' + h' - 1]])

day3p2 :: IO ()
day3p2 = runOnFile (head . day3 . claims) "day3.txt"
  where
    claims :: String -> [(String, [(Int, Int)])]
    claims = fmap (readClaim . words) . lines
    day3 xs = fmap fst .
        filter (\(cNum, q) -> null (fromList q `intersection` overlaping)) $ xs
      where
        overlaping :: Set (Int, Int)
        overlaping = fromList . concat . filter (\xs -> length xs > 1) . group .
            sort . concatMap snd $ xs

day4p1 :: IO ()
day4p1 = runOnFile day4 "day4.txt"
  where
    day4 =
        (\(guard, naps, _) -> guard * (snd . head . maximumBy (\a b -> compare (length a) (length b)) . group . sort . concatMap rangeMinutes $ naps)) .
        maximumBy (\(_, _, a) (_, _, b) -> compare a b) .
        addNaps .
        fmap (\xs -> (fst (head xs), concatMap snd xs)) .
        groupBy (\a b -> fst a == fst b) .
        (\(Right xs) -> sortBy (\a b -> compare (fst a) (fst b)) xs) .
        parse pShifts "Parsing Guard Shifts" .
        (\(Right xs) -> sort xs) .
        parse pLogFile "Parsing Log File"
    addNaps = fmap (\(guard, naps) -> (guard, naps, sum . fmap napLength $ naps))

day4p2 :: IO ()
day4p2 = runOnFile f "day4.txt"
  where
    f = calcNum . mergeGuard . mergeNaps . groupByGuard . sortByGuard .
        parseShifts . sortByTimeStamp . parseLogFile
    parseLogFile = parse pLogFile "Parsing Log File"
    sortByTimeStamp (Right xs) = sort xs
    parseShifts = parse pShifts "Parsing Guard Shifts"
    sortByGuard (Right xs) = sortBy (\a b -> compare (fst a) (fst b)) xs
    groupByGuard = groupBy (\a b -> fst a == fst b)
    mergeNaps = fmap (\xs -> (fst (head xs), concatMap rangeMinutes . concatMap snd $ xs))
    mergeGuard = maximumBy (\(_, (b, _)) (_, (e, _)) -> compare b e) . fmap (\(guard, naps) -> (guard, foo . maximumBy (\a b -> compare (length a) (length b)) . group . sort $ naps)) . filter (\(_, naps) -> not (null naps))
    calcNum (guard, (count, (hour, minute))) = guard * minute

foo xs = (length xs, head xs)

napLength :: (TimeStamp, TimeStamp) -> Integer
napLength (start, end) = toJulianSecond end - toJulianSecond start

rangeMinutes :: (TimeStamp, TimeStamp) -> [(Int, Int)]
rangeMinutes (start, end) = fmap (f . toJulianTime) [toJulianSecond start..toJulianSecond end - 1]
  where
    f (_, h, m) = (h, m)

toJulianSecond :: TimeStamp -> Integer
toJulianSecond TimeStamp { day=d, hour=h, minute=m } =
    (24 * 60 * toModifiedJulianDay d) + 60 * fromIntegral h + fromIntegral m

toJulianTime :: Integer -> (Day, Int, Int)
toJulianTime j =
    ( ModifiedJulianDay { toModifiedJulianDay = div j (24 * 60) }
    , fromIntegral (div (mod j (24 * 60)) 60)
    , fromIntegral (mod j 60)
    )

data TimeStamp
    = TimeStamp
    { day :: Day
    , hour :: Int
    , minute :: Int
    } deriving (Eq, Ord, Show)

data LogTok
    = BeginShiftTok
    { timeStamp :: TimeStamp
    , idNum :: Int }
    | WakeUpTok
    { timeStamp :: TimeStamp }
    | FallAsleepTok
    { timeStamp :: TimeStamp
    } deriving (Eq, Show)

instance Ord LogTok
  where
    compare a b = compare (getTimeStamp a) (getTimeStamp b)

getTimeStamp :: LogTok -> TimeStamp
getTimeStamp BeginShiftTok { timeStamp = timeStamp } = timeStamp
getTimeStamp WakeUpTok { timeStamp = timeStamp } = timeStamp
getTimeStamp FallAsleepTok { timeStamp = timeStamp } = timeStamp

pLogLine :: GenParser Char st LogTok
pLogLine =
    pTimeStamp >>= \ts ->
    char ' ' *>
    (
        (
            string "Guard #" *> many1 digit >>= \i ->
            string " begins shift\n" $> BeginShiftTok {
                timeStamp = ts,
                idNum = read i
            }
        ) <|>
        string "falls asleep\n" $> FallAsleepTok { timeStamp = ts } <|>
        string "wakes up\n" $> WakeUpTok { timeStamp = ts }
    )

pTimeStamp :: GenParser Char st TimeStamp
pTimeStamp =
    char '[' *> many1 digit >>= \y ->
    char '-' *> many1 digit >>= \mo ->
    char '-' *> many1 digit >>= \d ->
    char ' ' *> many1 digit >>= \h ->
    char ':' *> many1 digit >>= \mi ->
    char ']' $> TimeStamp {
        day = fromGregorian (read y) (read mo) (read d),
        hour = read h,
        minute = read mi
    }

pLogFile :: GenParser Char st [LogTok]
pLogFile = many pLogLine <* eof

satisfyT :: (Show t, Stream s m t) => (t -> Bool) -> ParsecT s u m t
satisfyT p = tokenPrim show nextPos testTok
  where
    updatePosTok pos c _cs = updatePosChar pos c
    nextPos pos x xs = pos -- don't care about pos
    testTok t        = if p t then Just t else Nothing
    updatePosChar a = a

pBeginShift :: GenParser LogTok st LogTok
pBeginShift = satisfyT (\x -> case x of BeginShiftTok {} -> True; _ -> False)

pFallAsleep :: GenParser LogTok st LogTok
pFallAsleep = satisfyT (\x -> case x of FallAsleepTok {} -> True; _ -> False)

pWakeUp :: GenParser LogTok st LogTok
pWakeUp = satisfyT (\x -> case x of WakeUpTok {} -> True; _ -> False)

pNap :: GenParser LogTok st (TimeStamp, TimeStamp)
pNap =
    pFallAsleep >>= \FallAsleepTok { timeStamp = startTime } ->
    pWakeUp >>= \WakeUpTok { timeStamp = endTime } ->
    return (startTime, endTime)

pShift :: GenParser LogTok st (Int, [(TimeStamp, TimeStamp)])
pShift =
    pBeginShift >>= \BeginShiftTok { idNum = idNum } ->
    many pNap >>= \naps ->
    return (idNum, naps)

pShifts :: GenParser LogTok st [(Int, [(TimeStamp, TimeStamp)])]
pShifts = many pShift

day5p1 :: IO ()
day5p1 = runOnFile (head . fmap (length . day5 []) . lines) "day5.txt"
p x y = toUpper x == toUpper y && x /= y
day5 :: String -> String -> String
day5 ps [] = reverse ps
day5 ps [x] = day5 (x : ps) []
day5 ps (x:xs@(y:ys)) =
    if p x y
    then let (ps', ys') = removePrev ps ys in day5 ps' ys'
    else day5 (x : ps) xs
removePrev xs@(x:xxs) ys@(y:yys)
    | p x y = removePrev xxs yys
    | otherwise = (xs, ys)
removePrev xs ys = (xs, ys)

day5p2 :: IO ()
day5p2 = runOnFile d "day5.txt"
  where
    d xs = let x = head . lines $ xs
           in length . minimumBy (\a b -> compare (length a) (length b)) . fmap (\letter -> day5 [] . filter (\c -> letter /= toUpper c) $ x) $ alphabet
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
