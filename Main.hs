module Main where
import Data.Char
import Debug.Trace
import Data.List
import System.Random
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Data.Maybe
import System.Exit
import Chopsticks
import System.Environment
import System.IO

data Flag = Help | Quick | Number String | Start String deriving (Eq, Show)
options :: [OptDescr  Flag]
options = [ Option ['h'] ["help"] (NoArg Help) "Print usage information and exit."
          , Option ['q'] ["quick","quiet"] (NoArg Quick) "Print the fortunes and exit."
          , Option ['n'] ["num"] (ReqArg Number "<num>") "Print <num> fortunes at a time."
          , Option ['s'] ["start"] (ReqArg Start "<num>") 
                "Start at fortune <num>. Defaults to value based on name, or 1 if quick mode."
          ]

main :: IO ()
main = do
    args <- getArgs
    let (flags, inputs, errors) =  getOpt Permute options args
    let fileName = if null inputs then "Game_Log.txt" else head inputs
    game <- loadGame fileName
    putBestMove game
