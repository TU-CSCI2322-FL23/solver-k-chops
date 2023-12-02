module ReadWrite where
import Chopsticks
import Solver
import Data.List
import Data.List.Split
import Text.Read (readMaybe)

readGame :: String -> Maybe Game
readGame str =
   case (splitOn ";" str) of
      [p1N, p2N, p1HandsStr, p2HandsStr, turnStr, countStr] ->
          do p1Hands <- readHands p1HandsStr
             p2Hands <- readHands p2HandsStr
             turnVal <- readPlayer turnStr
             countVal <- readMaybe countStr
             Just Game {p1Name = p1N
                       ,p2Name = p2N
                       ,playerOne = p1Hands
                       ,playerTwo = p2Hands
                       ,turn = turnVal
                       ,turnCount = countVal
                       }
      _ -> Nothing
     where readPlayer "1" = Just PlayerOne
           readPlayer "2" = Just PlayerTwo
           readPlayer _ = Nothing
           readHands str = if str == "" then Just [] else sequence $ map readMaybe (splitOn "," str)                        

showGame :: Game -> String
showGame game =
     let showHands lst = intercalate "," $ map show lst
         p1Hands = showHands $ playerOne game
         p2Hands = showHands $ playerTwo game
         t = if turn game == PlayerOne then "1" else "2"
    in intercalate ";" [p1Name game, p2Name game, p1Hands, p2Hands, t, show $ turnCount game]
--showGame takes a game and provides a string that describes the game fully
--Ex: showGame game = "Ashwin;Josh;1,1,1,1;1,1,1,1;1;50"
                
writeGame :: Game -> FilePath -> IO ()
writeGame game fileName =
    let
        gameString = showGame game
    in
        writeFile fileName gameString

loadGame :: FilePath -> IO (Maybe Game)
loadGame fileName = 
    do
        gameString <- readFile fileName
        return (readGame gameString) 

putBestMove :: Game -> IO ()
putBestMove game = 
    do
        putStrLn (show(bestMove game))