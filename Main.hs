{-# LANGUAGE BlockArguments #-}
module Main where
import Data.Char
import Debug.Trace
import Data.List
import Data.List.Split
import System.Random
import System.Console.GetOpt
import Control.Monad
import Data.Maybe
import System.Exit
import Chopsticks
import System.Environment
import System.IO


data Flag = HelpFlag | WinnerFlag | DepthFlag String | MoveFlag String | VerboseFlag deriving(Eq, Show)
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg HelpFlag) "Print flag usage information and exits",
            Option ['w'] ["winner"] (NoArg WinnerFlag) "Prints out the best move with no cut-off depth",
            Option ['d'] ["depth"] (ReqArg DepthFlag "<num>") "Allows user to specify <num> as a cutoff depth",
            Option ['m'] ["move"] (ReqArg MoveFlag "<move>") "Applies <move> to the current game and prints out the resulting game state (-m \"Add 0 0\" or -m \"Split\")",
            Option ['v'] ["verbose"] (NoArg VerboseFlag) "Outputs the move and a description of how good it is: win, lose, tie, or a rating"
          ]

main :: IO ()
main =
   do
      args <- getArgs
      let (flags, inputs, errors) =  getOpt Permute options args
      let fileName = if null inputs then "Game_Log.txt" else head inputs
      game <- loadGame fileName
      mapM_ (callCorrectFlag game) flags
      print (bestMove game)

readMove :: String -> Move
readMove "Split" = Split
readMove move =
   case words move of
   ["Add", x, y]  -> Add (read x) (read y)
   _              -> error "not a valid move"

callCorrectFlag :: Game -> Flag -> IO ()
callCorrectFlag game flag =
   case flag of
      HelpFlag          -> callHelpFlag
      WinnerFlag        -> callWinnerFlag game
      DepthFlag depth   -> callDepthFlag game (read depth)
      MoveFlag move     -> callMoveFlag game (readMove move)
      VerboseFlag       -> callVerboseFlag game

callHelpFlag :: IO ()
callHelpFlag =
   do
      putStrLn $ usageInfo "Chopsticks [options] [file]" options

callWinnerFlag :: Game -> IO ()
callWinnerFlag game =
   do
      print $ bestMove game

callDepthFlag :: Game -> Int -> IO ()
callDepthFlag game depth =
      do
         print depth --placeholder until I get Ashwin's function
      -- use (read DepthFlag) as cut-off param for new bestMove
           

callMoveFlag :: Game -> Move -> IO ()
callMoveFlag game move =
   do
      case makeMove game move of
         Nothing -> error "not a valid move"
         _ -> print $ makeMove game move
--not sure if the move flag needs to update the game or just print the game state if the move was made
--if it needs to update the game, maybe use writeGame game file 

callVerboseFlag :: Game -> IO ()
callVerboseFlag game = do print "verbose" --placeholder until I get the rateGame
   --do
      --case rateGame of
         --case for if move leads to winner
         --case for if move leads to loser
         --case for if move leads to tie
         --case for game rating
      --confused as to what it means by move






