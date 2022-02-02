module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.Char
import Data.List
import Data.Ord
import System.Exit
import System.IO
import System.Random
import Data.List.Split
import Data.Time.Clock.POSIX

getWords :: IO [String]
getWords = do
  words <- readFile "words.txt"
  return [w | w <- lines words, length w == 5, all (`elem` ['a' .. 'z']) w]

type GoodLetters = [Char]
type BadLetters  = [Char]
type PosLetters  = [Char]
type NPosLetters = [[Char]]

chooseRandom :: [String] -> IO String
chooseRandom lst = do
  t <- (fromIntegral . round <$> getPOSIXTime) :: IO Int
  let (i, _) = uniformR (0, length lst - 1) (mkStdGen t)
  putStrLn $ "number of possible words = " ++ show (length lst)
  return $ lst !! i

getFirstTry :: IO String
getFirstTry = do
  wrds <- getWords
  let wordcounts  = [ (head c, length c) | c <- group $ sort $ mconcat wrds ]
      descSort :: [(a, Int)] -> [(a, Int)]
      descSort    = sortOn (Down . snd)
      freqLetters = descSort wordcounts
      scanSet     = fst <$> take 10 freqLetters
      scores      = descSort [ (w, length (nub $ intersect scanSet w)) | w <- wrds ]
      candidates  = [ fst w | w <- scores, snd w == 5 ]
  chooseRandom candidates

posMatch :: String -> String -> Bool
posMatch [] []   = True
posMatch [] _    = False
posMatch _ []    = False
posMatch [' '] _ = True
posMatch (' ':xs) (_:ys) = posMatch xs ys
posMatch (x:xs) (y:ys) = x == y && posMatch xs ys

anyPosMatch :: [String] -> String -> Bool
anyPosMatch posList key = or cklst
  where
    posns = transpose posList
    cklst = uncurry elem <$> zip key posns

data WordStats = WordStats
  { posLetters  :: PosLetters
  , nposLetters :: NPosLetters
  , goodLetters :: GoodLetters
  , badLetters  :: BadLetters
  } deriving (Eq, Show)

resultToWordStats :: String -> String -> WordStats
resultToWordStats score tryWord =
  WordStats posLetters [nposLetters] goodLetters badLetters
  where
    score'      = toUpper <$> score
    decoded     = zip score' tryWord
    posLetters  = (\(x,y) -> if x == 'G' then y else ' ') <$> decoded
    nposLetters = (\(x,y) -> if x `elem` "BY" then y else ' ') <$> decoded
    goodLetters = snd <$> filter ((`elem` "GY") . fst) decoded
    badLetters' = snd <$> filter ((== 'B') . fst) decoded
    badLetters  = filter (`notElem` goodLetters) badLetters'

mergeWordStats :: WordStats -> WordStats -> WordStats
mergeWordStats w1 w2 = statsMerged
  where
    statsMerged = WordStats
      { posLetters  = zipWith (\a b -> if a /= ' ' then a else b) (posLetters w2) (posLetters w1)
      , nposLetters = nposLetters w2 ++ nposLetters w1
      , goodLetters = nub $ goodLetters w1 ++ goodLetters w2
      , badLetters  = nub $ badLetters w1 ++ badLetters w2
      }

exitIfDone :: String -> IO ()
exitIfDone s = when (null s) exitSuccess

chooseWord :: WordStats -> IO String
chooseWord ws = do
  dict <- getWords
  chooseRandom [ w | w <- filteredDict dict, ckBad w, ckGood w ]
  where
    WordStats
      { posLetters  = posLetters'
      , nposLetters = nposLetters'
      , goodLetters = goodLetters'
      , badLetters  = badLetters'
      } = ws
    dict' x = if null nposLetters' then x else filter (not . anyPosMatch nposLetters') x
    filteredDict x = if null posLetters' then dict' x else filter (posMatch posLetters') (dict' x)
    ckBad :: String -> Bool
    ckBad = not . any (`elem` badLetters')
    ckGood :: String -> Bool
    ckGood w = length goodLetters' == length (goodLetters' `intersect` w)

runLoopSt :: String -> StateT WordStats IO ()
runLoopSt lastTry = do
  liftIO $ putStr "enter result:  "
  result <- liftIO getLine
  liftIO $ exitIfDone result
  liftIO $ putStrLn []
  let stats = resultToWordStats result lastTry
  modify (mergeWordStats stats)
  newStats <- get
  --liftIO $ print newStats
  guess <- liftIO $ chooseWord newStats
  liftIO $ putStr "\ncurrent guess: "
  liftIO $ putStrLn guess
  runLoopSt guess

initWordStats :: WordStats
initWordStats = WordStats "     " [] [] []

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  firstTry <- getFirstTry
  putStrLn $ "initial guess: " ++ firstTry
  void $ runStateT (runLoopSt firstTry) initWordStats
