module Lib where

import System.IO (openFile, hGetContents, hClose, IOMode(..))

import Data.List
    ( find
    , group
    , groupBy
    , sort
    , sortBy
    , sortOn
    , minimumBy
    , maximumBy
    , filter
    , elemIndex
    , concat
    , (\\)
    , nub
    , delete
    , union
    , partition
    , deleteFirstsBy
    , foldl'
    , unfoldr
    , intercalate
    )
import Data.Map.Strict (Map)
import qualified Data.Map as Map
import Data.Functor (($>))
import Data.Foldable (toList)
import Data.Maybe (maybeToList, fromMaybe, isJust, fromJust, maybe)
import Data.Either (fromRight)
import Data.Set (Set, fromList, intersection, insert, member, empty, size)
import Data.Char (isDigit, toUpper, ord)
import Data.Time.Calendar (Day(..), fromGregorian)

import Control.Monad ((<=<))
import Control.Applicative((<$>))

import Text.Parsec.Prim (ParsecT, Stream)
import Text.ParserCombinators.Parsec
    ( GenParser
    , many
    , oneOf
    , many1
    , tokenPrim
    , eof
    , char
    , digit
    , string
    , (<|>)
    , parse
    , spaces
    , count
    , option
    )

someFunc :: IO ()
someFunc = day11p2

runOnFile :: Show a => (String -> a) -> String -> IO ()
runOnFile f fname =
    openFile fname ReadMode >>= \handle ->
    hGetContents handle >>= \contents ->
    putStrLn (show (f contents)) *>
    hClose handle

runOnFile2 :: (String -> String) -> String -> IO ()
runOnFile2 f fname =
    openFile fname ReadMode >>= \handle ->
    hGetContents handle >>= \contents ->
    putStrLn (f contents) *>
    hClose handle

-- > Day 1

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

-- > Day 2

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

-- > Day 3

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

-- > Day 4

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
    mergeGuard = maximumBy (\(_, (b, _)) (_, (e, _)) -> compare b e) .
                 fmap (\(guard, naps) -> (guard, foo . maximumBy (\a b -> compare (length a) (length b)) . group . sort $ naps)) .
                 filter (\(_, naps) -> not (null naps))
    calcNum (guard, (count, (hour, minute))) = guard * minute

foo xs = (length xs, head xs)

napLength :: (TimeStamp, TimeStamp) -> Integer
napLength (start, end) = toJulianSecond end - toJulianSecond start

rangeMinutes :: (TimeStamp, TimeStamp) -> [(Int, Int)]
rangeMinutes (start, end) = fmap (f . toJulianTime) [toJulianSecond start..toJulianSecond end - 1]
  where
    f (_, h, m) = (h, m)

toJulianSecond :: TimeStamp -> Integer
toJulianSecond TimeStamp { day = d, hour = h, minute = m } =
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

-- > Day 5

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
           in length . minimumBy (\a b -> compare (length a) (length b)) .
              fmap (\letter -> day5 [] . filter (\c -> letter /= toUpper c) $ x) $
              alphabet
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- > Day 6

day6p1 :: IO ()
day6p1 = runOnFile (part1 . parseDay6) "day6.txt"
  where
    part1 points = length . maximumBy (\a b -> compare (length a) (length b)) .
                   filter (not . null) . fmap (\(Just x) -> toList x) .
                   filter isJust . fmap (getArea points (getEdge points)) $
                   points

parseDay6 :: String -> [Tile]
parseDay6 = fromRight [] . parse pTileFile "Parsing Tile File"

data Tile = Tile Int Int deriving (Eq, Ord, Show)
data Edge = Edge Int Int Int Int deriving Show

pTileFile :: GenParser Char st [Tile]
pTileFile = many pTile

pTile :: GenParser Char st Tile
pTile = many1 digit >>= \x -> string ", " *> many1 digit >>= \y -> char '\n' $>
        Tile (read x) (read y)

getEdge :: [Tile] -> Edge
getEdge ts = Edge (minimum xs) (minimum ys) (maximum xs) (maximum ys)
  where
    xs = fmap (\(Tile x _) -> x) ts
    ys = fmap (\(Tile _ y) -> y) ts

isInside :: Edge -> Tile -> Bool
isInside (Edge minX minY maxX maxY)
         (Tile x y) = x > minX && x < maxX && y > minY && y < maxY

getDistance :: Tile -> Tile -> Int
getDistance (Tile x1 y1) (Tile x2 y2) = abs (x1 - x2) + abs (y1 - y2)

getNearestOne :: [Tile] -> Tile -> Maybe Tile
getNearestOne points tile = onlyOneNearest
  where
    distances = fmap (`getDistance` tile) points
    minimumDistance = minimum distances
    nearestPoints = fmap fst . filter eqMinimumDistance $ zip points distances
    eqMinimumDistance (_, distance) = distance == minimumDistance
    onlyOneNearest = if null . tail $ nearestPoints
                     then Just (head nearestPoints)
                     else Nothing

getArea :: [Tile] -> Edge -> Tile -> Maybe (Set Tile)
getArea points edge origin = search origin empty
  where
    search tile@(Tile x y) visitedTiles
        | member tile visitedTiles = Just visitedTiles -- was here already
        | Just origin == getNearestOne points tile =
            if isInside edge tile                      -- origin is nearest
            then search (Tile (x - 1) y) <=<
                 search (Tile (x + 1) y) <=<
                 search (Tile x (y - 1)) <=<
                 search (Tile x (y + 1)) $
                 insert tile visitedTiles
            else Nothing                               -- it's infinite
        | otherwise = Just visitedTiles

day6p2 :: IO ()
day6p2 = runOnFile (part2 . parseDay6) "day6.txt"
  where
    part2 points = fmap size . find (not . null) .
                   fmap (getNearestArea points) $
                   points

getDistanceTotal :: [Tile] -> Tile -> Int
getDistanceTotal points tile = sum . fmap (`getDistance` tile) $ points

getNearestArea :: [Tile] -> Tile -> Set Tile
getNearestArea points origin = search origin empty
  where
    search tile@(Tile x y) visitedTiles
        | member tile visitedTiles = visitedTiles -- was here before
        | getDistanceTotal points tile <= 10000 =
            search (Tile (x - 1) y) .
            search (Tile (x + 1) y) .
            search (Tile x (y - 1)) .
            search (Tile x (y + 1)) $
            insert tile visitedTiles
        | otherwise = visitedTiles

-- > Day 7

day7p1 :: IO ()
day7p1 = runOnFile (part1 . parseDay7) "day7.txt"
  where
    part1 deps = sNode :
        search [sNode] (fmap (\to -> ("",to)) (delete sNode sFrom) ++ sTo)
      where
        sFrom = (sort . nub . fmap fst $ deps) \\ (sort . nub . fmap snd $ deps)
        sTo = fmap (\xs -> (sort . fmap fst $ xs, snd . head $ xs)) .
              groupBy (\(_, a) (_, b) -> a == b) . sortOn snd $ deps
        sNode = minimum sFrom
        search _ [] = []
        search from to = snd next : search (snd next : from) (delete next to)
          where
            next = head . sortOn snd . filter (\(f, _) -> all (`elem` from) f) $
                   to

type Dep = (Char, Char)

parseDay7 :: String -> [Dep]
parseDay7 = fromRight [] . parse pDepFile "Parsing Dependency File"

pDepFile :: GenParser Char st [Dep]
pDepFile = many pDep

pNode :: GenParser Char st Char
pNode = oneOf "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

pDep :: GenParser Char st Dep
pDep = string "Step " *> pNode >>= \a ->
       string " must be finished before step "  *> pNode >>= \b ->
       string " can begin.\n" $> (a, b)

day7p2 :: IO ()
day7p2 = runOnFile (part2 . parseDay7) "day7.txt"
  where
    part2 deps = search 0 [] 5 [] (fmap (\to -> ("",to)) sFrom ++ sTo)
      where
        sFrom = (sort . nub . fmap fst $ deps) \\ (sort . nub . fmap snd $ deps)
        sTo = fmap (\xs -> (sort . fmap fst $ xs, snd . head $ xs)) .
              groupBy (\(_, a) (_, b) -> a == b) . sortOn snd $ deps
        search :: Int -> String -> Int -> [(Int, Char)] -> [(String, Char)] -> Int
        search n _ _ [] [] = n - 1
        search n from idling working to =
            search (n + 1) from' idling' working' to'
          where
            doneAndWorking :: ([(Int, Char)], [(Int, Char)])
            doneAndWorking = partition (\(i, c) -> i == 0) . fmap (\(i, c) -> (i - 1, c)) $ working
            from' :: String
            from' = fmap snd (fst doneAndWorking) ++ from
            availableWorkers :: Int
            availableWorkers = length (fst doneAndWorking) + idling
            availableNodes :: [(String, Char)]
            availableNodes = sortOn snd . filter (\(f, _) -> all (`elem` from') f) $ to
            idling' :: Int
            idling' = availableWorkers - min availableWorkers (length availableNodes)
            readyTo :: [(String, Char)]
            readyTo = take availableWorkers availableNodes
            working' :: [(Int, Char)]
            working' = fmap (\(_, node) -> (time node, node)) readyTo ++ snd doneAndWorking
            to' :: [(String, Char)]
            to' = deleteFirstsBy (==) to readyTo
            time :: Char -> Int
            time c = ord c - ord 'A' + 61

-- > Day 8

day8p1 :: IO ()
day8p1 = runOnFile (metaSum . alwaysRight . pDay8) "day8.txt"
  where
    metaSum (Tree children meta) = sum meta + sum (fmap metaSum children)

data Tree = Tree [Tree] [Int] deriving Show

pDay8 = parse pTree "Failed Parsing"

pNum :: GenParser Char st Int
pNum = read <$> many1 digit <* spaces

pTree :: GenParser Char st Tree
pTree = pNum >>= \nChildren ->
        pNum >>= \nMeta ->
        count nChildren pTree >>= \children ->
        count nMeta pNum >>= \meta ->
        return (Tree children meta)

day8p2 :: IO ()
day8p2 = runOnFile (metaSum . alwaysRight . pDay8) "day8.txt"
  where
    metaSum (Tree [] meta) = sum meta
    metaSum (Tree children meta) =
        sum . concatMap (fmap metaSum . get children) $ meta
    get xs n = take 1 . drop (n - 1) $ xs

alwaysRight :: Either a b -> b
alwaysRight (Right b) = b

-- > Day 9

day9p1 = maximum . Map.elems . scores . fromJust .
         find (\game -> nextMarble game >= 71790) $
         scanl (\game _ -> takeTurn game) (newGame 459) [1..]

day9p2 = maximum . Map.elems . scores . fromJust .
         find (\game -> nextMarble game >= 7179000) $
         scanl (\game _ -> takeTurn game) (newGame 459) [1..]

type Position = Int
type Player = Int
type NumPlayers = Int
type Marble = Int
type UntilMarble = Int
type Score = Int
type Scores = Map Player Score
data Board = Board [Marble] Marble [Marble] deriving Show
data Game = Game
    { numPlayers :: NumPlayers
    , nextPlayer :: Player
    , nextMarble :: Marble
    , board :: Board
    , scores :: Scores
    } deriving Show

newGame :: NumPlayers -> Game
newGame numPlayers
    = Game { numPlayers = numPlayers
           , nextPlayer = 0
           , nextMarble = 1
           , board = Board [] 0 []
           , scores = score'
           }
  where
    score' = Map.fromList [(player, 0) | player <- [0 .. numPlayers - 1]]

takeTurn :: Game -> Game
takeTurn game
    | mod (nextMarble game) 23 == 0
        = game { nextPlayer = nextPlayer'
               , nextMarble = nextMarble'
               , board = removeCurrent slideLeft
               , scores = scores'
               }
    | otherwise
        = game { nextPlayer = nextPlayer'
               , nextMarble = nextMarble'
               , board = insertRight (nextMarble game) . moveRight . board $
                         game
               }
  where
    nextPlayer' = mod (nextPlayer game + 1) (numPlayers game)
    nextMarble' = nextMarble game + 1
    scores' = Map.adjust
                  (+ (nextMarble game + getCurrentMarble slideLeft))
                  (nextPlayer game)
                  (scores game)
    slideLeft = moveLeft . moveLeft . moveLeft . moveLeft . moveLeft .
                moveLeft . moveLeft . board $ game

getCurrentMarble :: Board -> Marble
getCurrentMarble (Board _ marble _) = marble

moveRight :: Board -> Board
moveRight board@(Board [] _ []) = board
moveRight (Board left marble [])
    = Board [] (head right) (drop 1 right)
  where
    right = foldl' (flip (:)) [marble] left
moveRight (Board left leftHead (marble:right))
    = Board (leftHead:left) marble right

moveLeft :: Board -> Board
moveLeft board@(Board [] _ []) = board
moveLeft (Board [] marble right)
    = Board (drop 1 left) (head left) []
  where
    left = foldl' (flip (:)) [marble] right
moveLeft (Board (marble:left) rightHead right)
    = Board left marble (rightHead:right)

insertRight :: Marble -> Board -> Board
insertRight marble (Board left leftHead right)
    = Board (leftHead:left) marble right

removeCurrent :: Board -> Board
removeCurrent (Board left _ [])
    = Board [] (head right) (drop 1 right)
  where
    right = reverse left
removeCurrent (Board left _ (headRight:right))
    = Board left headRight right

-- Day 10

day10p1 :: IO () -- intercalate "\n\n\n" . fmap graphPV . reverse . part1
day10p1 = runOnFile2 (graphPV . last . part1 . alwaysRight . pDay10) "day10.txt"
  where
    part1 = unfoldr f
    f :: [PV] -> Maybe ([PV], [PV])
    f pvs
        | a < a' = Nothing
        | otherwise = Just (pvs', pvs')
      where
        pvs' = fmap nextPV pvs
        a = area pvs
        a' = area pvs'

day10p2 :: IO ()
day10p2 = runOnFile2 (show . length . part2 . alwaysRight . pDay10) "day10.txt"
    where
    part2 = unfoldr f
    f :: [PV] -> Maybe ([PV], [PV])
    f pvs
        | a < a' = Nothing
        | otherwise = Just (pvs', pvs')
        where
        pvs' = fmap nextPV pvs
        a = area pvs
        a' = area pvs'

minx :: [PV] -> Int
minx = minimum . fmap (\(PV x _ _ _) -> x)
maxx :: [PV] -> Int
maxx = maximum . fmap (\(PV x _ _ _) -> x)
miny :: [PV] -> Int
miny = minimum . fmap (\(PV _ y _ _) -> y)
maxy :: [PV] -> Int
maxy = maximum . fmap (\(PV _ y _ _) -> y)
width :: [PV] -> Int
width pvs = maxx pvs - minx pvs
height :: [PV] -> Int
height pvs = maxy pvs - miny pvs
area :: [PV] -> Integer
area pvs = fromIntegral (height pvs) * fromIntegral (width pvs)

pDay10 = parse pDay10File "Parsing Vector File"

nextPV :: PV -> PV
nextPV (PV x y dX dY) = PV (x + dX) (y + dY) dX dY

graphHor :: Int -> [PV] -> String
graphHor y pvs =
    reverse . snd . foldl' f (minx pvs - 1, "") . nub . fmap (\(PV x _ _ _) -> x) .
    sortOn (\(PV x _ _ _) -> x) . filter (\(PV _ y' _ _) -> y == y') $
    pvs
  where
    f (x, cs) x' = (x', '#' : replicate (x' - x - 1) '.' ++ cs)

graphPV :: [PV] -> String
graphPV pvs = intercalate "\n" . fmap (`graphHor` pvs) $ [miny pvs..maxy pvs]

type Vec = (Int, Int)
data PV = PV Int Int Int Int deriving Show

pDay10File :: GenParser Char st [PV]
pDay10File = many pPV

pSInt :: GenParser Char st Int
pSInt = option 1 (char '-' $> (-1)) >>= \s ->
        many1 digit >>= \ds ->
        return (s * read ds)

pVec = char '<' *> spaces *> pSInt >>= \x ->
       char ',' *> spaces *> pSInt >>= \y ->
       char '>' $> (x, y)

pPV :: GenParser Char st PV
pPV = string "position=" *> pVec >>= \(px, py) ->
      string " velocity=" *> pVec >>= \(vx, vy) ->
      char '\n' $> PV px py vx vy

-- Day 11

day11p1 :: IO ()
day11p1 = print part1
  where
    part1 = fmap snd . maximumBy (\(a,_) (b,_) -> compare a b) . fmap (\x -> (sqrLvl x, x)) $ [(x, y) | x <- [1..300 - 2], y <- [1..300 - 2]]
    sqrLvl (oX, oY) = sum $ powLvl <$> [(x, y) | x <- [oX..oX + 2], y <- [oY..oY + 2]]

serNo = 7803

powLvl (x, y) = mod (div (p - mod p 100) 100) 10 - 5
  where
    rackId = x + 10
    p = (rackId * y + serNo) * rackId

day11p2 :: IO ()
day11p2 = print part2
  where
    part2 = maximumBy (\(a,_) (b,_) -> compare a b) $ (\a -> (sqrLvl a, a)) <$> [(x, y, s) | s <- [1..300], x <- [1..300 - s + 1], y <- [1..300 - s + 1]]
    sqrLvl (oX, oY, s) = sum $ powLvl <$> [(x, y) | x <- [oX..oX + (s - 1)], y <- [oY..oY + (s - 1)]]

-- cacheFun :: Ord k => (k -> a) -> [k] -> (k -> a) -- Map k a
-- cacheFun f ks = \k -> fromJust (Map.lookup k km)
--   where
--     km = Map.fromList $ (\k -> (k, f k)) <$> ks
