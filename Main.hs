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
-- import Test.Grader.Timing (BlowupReport(flag))
-- import System.Win32 (COORD(yPos))
-- import Distribution.Simple.CCompiler (filenameCDialect)


-- data Flag = Flag {
--    flagHelp       :: Bool,
--    flagWinner     :: Bool,
--    flagDepth      :: Int,
--    flagMove       :: Maybe Move,
--    flagVerbose    :: Bool
-- }

-- defaultFlags :: Flag
-- defaultFlags = Flag{
--    flagHelp       = False,
--    flagWinner     = False,
--    flagDepth      = 0,
--    flagMove       = Nothing,
--    flagVerbose    = False
-- }


data Flag = HelpFlag | WinnerFlag | DepthFlag String | MoveFlag String | VerboseFlag deriving(Eq, Show)
options :: [OptDescr Flag]
options = [ Option ['h'] ["help"] (NoArg HelpFlag) "Print usage information and exit.",
            Option ['w'] ["winner"] (NoArg WinnerFlag) "Prints out the best move with no cut-off depth",
            Option ['d'] ["depth"] (ReqArg DepthFlag "<num>") "Allows user to specify <num> as a cutoff depth",
            Option ['m'] ["move"] (ReqArg MoveFlag "<move>") "Applies <move> to the current game and prints out the resulting game state (formatted like -m Add 0 0 or -m Split)",
            Option ['v'] ["verbose"] (NoArg VerboseFlag) "Outputs the move and a description of how good it is: win, lose, tie, or a rating"
          ]

readMove :: String -> Move
readMove "Split" = Split
readMove move = 
   case words move of
   ["Add", x, y] -> Add (read x) (read y)
   _ -> error "not a valid move"



main :: IO ()
main = do
   args <- getArgs
   let (flags, inputs, errors) =  getOpt Permute options args
   let fileName = if null inputs then "Game_Log.txt" else head inputs
   if HelpFlag `elem` flags
      then callHelpFlag
   else  
      do game <- loadGame fileName
         case flags of 
            [WinnerFlag] -> callWinnerFlag game
            [DepthFlag depth] -> callDepthFlag game (read depth)
            [MoveFlag move] -> callMoveFlag game (readMove move) fileName
            
            _ -> putStrLn "hi"
         -- if WinnerFlag `elem` flags
         --   then 
         --        do game <- loadGame fileName
         --           putBestMove game
         --   else 
         --       putStrLn "hello bruh"
            --   if DepthFlag `elem` flags
            --   then putStrLn "hi" 
            --     else do game <- loadGame fileName
            --             putBestMove game--if DepthFlag `elem` flags
             --then -- use (read DepthFlag) as cut-off param for new bestMove
             --else -- new bestMove with depth as 5

--maybe do pattern matching but are calls to main allowed to have multiple flags?
-- if so we need to do pattern matching for all combinations which is kind of a lot

       
callHelpFlag :: IO ()
callHelpFlag = 
   do 
      putStrLn $ usageInfo "Chopsticks [options] [file]" options

callWinnerFlag :: Game -> IO ()
callWinnerFlag game = 
   do 
      print $ bestMove game

callDepthFlag :: Game -> Int -> IO () 
callDepthFlag game = 
      do
         print --placeholder until I get Ashwin's function
      -- use (read DepthFlag) as cut-off param for new bestMove

callMoveFlag :: Game -> Move -> FilePath -> IO ()
callMoveFlag game move file = 
   do 
      case makeMove game move of
         Nothing -> error "not a valid move"
         _ -> print $ makeMove game move
--not sure if the move flag needs to update the game or just print the game state if the move was made
--if it needs to update the game, maybe use writeGame game file 

callVerboseFlag :: Game -> Move -> IO ()
callVerboseFlag game move = undefined --placeholder until I get the rateGame
   --do
      --case rateGame of
         --case for if move leads to winner
         --case for if move leads to loser
         --case for if move leads to tie
         --case for game rating
      
      
   




