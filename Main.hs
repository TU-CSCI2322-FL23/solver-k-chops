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
            Option ['m'] ["move"] (ReqArg MoveFlag "<move>") "Applies <move> to the current game and prints out the resulting game state (formatted like -m (Add 0 0) or -m Split)",
            Option ['v'] ["verbose"] (NoArg VerboseFlag) "Outputs the move and a description of how good it is: win, lose, tie, or a rating"
          ]

readMove :: String -> Move
readMove "Split" = Split
readMove move = 
   case words move of
   ["Add", x, y] -> Add (read x) (read y)

-- applyFlag :: Game -> Flag -> Maybe Move
-- applyFlag game WinnerFlag = mapMaybe bestMove game   
-- readMove (applyFlag game WinnerFlag) 


main :: IO ()
main = do
   args <- getArgs
   let (flags, inputs, errors) =  getOpt Permute options args
   let fileName = if null inputs then "Game_Log.txt" else head inputs
   if HelpFlag `elem` flags
   then putStrLn $ usageInfo "Chopsticks [options] [file]" options
   else  
      do game <- loadGame fileName
         case flags of 
            [WinnerFlag] -> putBestMove game
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



        
