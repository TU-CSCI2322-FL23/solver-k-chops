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
import Solver
import ReadWrite
import System.Environment
import System.IO
import TestInputs

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
      case game of 
         Nothing -> putStrLn "Error, invalid game"
         Just game ->
            do
               if HelpFlag `elem` flags
                  then callHelpFlag
               else if WinnerFlag `elem` flags
                  then callWinnerFlag game
               else if any isMoveFlag flags
                     then let (MoveFlag m) = head $ filter isMoveFlag flags
                          in if (VerboseFlag) `elem` flags
                              then callVerboseFlag game (readMove m)
                             else callMoveFlag game (readMove m)
                    else if any isDepthFlag flags
                           then let (DepthFlag d) = head $ filter isDepthFlag flags
                                in  print $ whoMightWin game (read d)
                           else print $ whoMightWin game 3

isMoveFlag :: Flag -> Bool
isMoveFlag (MoveFlag m) = True
isMoveFlag anythingElse = False

isDepthFlag :: Flag -> Bool
isDepthFlag (DepthFlag d) = True
isDepthFlag anythingElse = False

readMove :: String -> Move
readMove "Split" = Split
readMove move =
   case words move of
   ["Add", x, y]  -> Add (read x) (read y)
   _              -> error "not a valid move"

callHelpFlag :: IO ()
callHelpFlag =
   do
      putStrLn $ usageInfo "Chopsticks [options] [file]" options

callWinnerFlag :: Game -> IO ()
callWinnerFlag game =
   do
      print $ bestMove game

           

callMoveFlag :: Game -> Move -> IO ()
callMoveFlag game move =
   do
      case makeMove game move of
         Nothing -> error "not a valid move"
         _ -> print $ makeMove game move


callVerboseFlag :: Game -> Move -> IO ()
callVerboseFlag game move = 
   do
      case makeMove game move of
         Nothing -> error "not a valid move"
         Just newGame -> do putStrLn $ show move
                            putStrLn $ show newGame
                            putStrLn $ show $ rateGame newGame
