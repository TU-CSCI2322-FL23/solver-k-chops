module Main where
import Data.Char
import Debug.Trace
import Data.List
import Data.List.Split
import System.Random
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Exit
import Chopsticks
import System.Environment
import System.IO

data Flag = Help | WinFlag | Depth String deriving(Eq, Show)
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit.",
            Option ['w'] ["winner"] (NoArg WinFlag) "Prints out the best move with no cut-off depth",
            Option ['d'] ["depth"] (ReqArg Depth "<num>") "Allows user to specify <num> as a cutoff depth"
          ]
main :: IO ()
main = do
    args <- getArgs
    let (flags, inputs, errors) =  getOpt Permute options args
    let fileName = if null inputs then "Game_Log.txt" else head inputs
    if Help `elem` flags
      then putStrLn $ usageInfo "Chopsticks [options] [file]" options
      else if WinFlag `elem` flags
           then do game <- loadGame fileName
                   putBestMove game
           else putStrLn "" --if Depth `elem` flags
             --then -- use (read Depth) as cut-off param for new bestMove
             --else -- new bestMove with depth as 5
