module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State
import Data.List
import System.Exit
import System.IO
import System.Random
import Data.List.Split
import Data.Time.Clock.POSIX

getWords :: IO [String]
getWords = do
  words <- readFile "/usr/share/dict/words"
  return [w | w <- lines words, length w == 5, all (`elem` ['a' .. 'z']) w]

freqWords :: [String]
freqWords = ["aisle","alert","alien","aliso","alist","alite","aloin","alone","alose","alter","altin","altun","alure","anile","anise","anoil","anole","anoli","antes","antre","arent","ariel","ariot","arise","arist","arite","arles","arnut","arose","arsle"]

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

resultToWordStats :: String -> WordStats
resultToWordStats input =
  WordStats posLetters [nposLetters] goodLetters badLetters
  where
    decoded     = chunksOf 2 input
    posLetters  = (\[x,y] -> if x == 'G' then y else ' ') <$> decoded
    nposLetters = (\[x,y] -> if x `elem` "BY" then y else ' ') <$> decoded
    goodLetters = mconcat $ tail <$> filter ((`elem` "GY") . head) decoded
    badLetters' = mconcat $ tail <$> filter ((== 'B') . head) decoded
    badLetters  = filter (`notElem` goodLetters) badLetters'

mergeWordStats :: WordStats -> WordStats -> WordStats
mergeWordStats w1 w2 = statsMerged
  where
    statsMerged = WordStats
      { posLetters  = zipWith (\a b -> if a /= ' ' then a else b) (posLetters w2) (posLetters w1)
      , nposLetters = nposLetters w2 ++ nposLetters w1
      , goodLetters = nub $ goodLetters w1 ++ goodLetters w2
      , badLetters  = badLetters  w1 ++ badLetters w2
      }

exitIfDone :: String -> IO ()
exitIfDone s = when (null s) exitSuccess

chooseWord :: WordStats -> IO String
chooseWord ws = do
  dict <- getWords
  chooseRandom [ w | w <- filteredDict dict, ckBad w, ckGood w ]
  where
    nposLetters' = nposLetters ws
    posLetters'  = posLetters ws
    goodLetters' = goodLetters ws
    badLetters'  = badLetters ws
    dict' x = if null nposLetters' then x else filter (not . anyPosMatch nposLetters') x
    filteredDict x = if null posLetters' then dict' x else filter (posMatch posLetters') (dict' x)
    ckBad :: String -> Bool
    ckBad = not . any (`elem` badLetters')
    ckGood :: String -> Bool
    ckGood w = length goodLetters' == length (group (sort w) `intersect` group (sort goodLetters'))

runLoopSt :: StateT WordStats IO ()
runLoopSt = do
  liftIO $ putStr "enter result: "
  result <- liftIO getLine
  liftIO $ exitIfDone result
  let stats = resultToWordStats result
  modify (mergeWordStats stats)
  newStats <- get
  --liftIO $ print newStats
  guess <- liftIO $ chooseWord newStats
  liftIO $ putStr "\ncurrent guess: "
  liftIO $ putStrLn $ guess ++ "\n"
  runLoopSt

initWordStats :: WordStats
initWordStats = WordStats "     " [] [] []

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  firstTry <- chooseRandom freqWords
  putStrLn $ "initial guess: " ++ firstTry ++ "\n"
  void $ runStateT runLoopSt initWordStats
