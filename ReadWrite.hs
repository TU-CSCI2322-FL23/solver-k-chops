module ReadWrite where
import Chopsticks
import Solver
import Data.List.Split
import Data.Maybe -- will be removed when fromJust is removed from loadGame

readGame :: String -> Maybe Game
readGame str = 
    if ((length $ filter (== ':') str) == 6 && (length $ filter (== ';') str) == 5)
        then readGameHelp 5 (map (break (== ':')) $ splitOn ";" str)
    else Nothing
    where readGameHelp :: Int -> [(String, String)] -> Maybe Game
          readGameHelp 0 [(key, value)] = 
            let count = read $ tail value  
            in  if (count <= 50) 
                    then Just Game {playerOne = [], playerTwo = [], p1Name = "", p2Name = "", turn = PlayerOne, turnCount = count}
                else Nothing
          readGameHelp 1 ((key, value):xs) = 
            let game = readGameHelp 0 xs
            in  case game of
                    Nothing -> Nothing
                    Just g -> let t = tail value
                              in  if (t == "PlayerOne") then Just g
                                  else if (t == "PlayerTwo") then Just $ g {turn = PlayerTwo} 
                                  else Nothing
          readGameHelp 2 ((key, value):xs) = 
            let game = readGameHelp 1 xs
            in  case game of
                    Nothing -> Nothing
                    Just g -> let handString = tail value
                                  contentsString = init $ tail handString
                                  hand = if (contentsString == "") then [] else map read (splitOn "," contentsString)
                              in  if ((head handString == '[') && (last handString == ']') && (all (\c -> c == ',' || c `elem` ['0','1','2','3','4','5','6','7','8','9']) contentsString)) 
                                      then Just $ g {playerTwo = hand}
                                  else Nothing
          readGameHelp 3 ((key, value):xs) = 
            let game = readGameHelp 2 xs
            in  case game of 
                    Nothing -> Nothing
                    Just g -> let handString = tail value
                                  contentsString = init $ tail handString
                                  hand = if (contentsString == "") then [] else map read (splitOn "," contentsString)
                              in  if ((head handString == '[') && (last handString == ']') && (all (\c -> c == ',' || c `elem` ['0','1','2','3','4','5','6','7','8','9']) contentsString)) 
                                      then Just $ g {playerOne = hand}
                                  else Nothing
          readGameHelp 4 ((key, value):xs) = 
            let game = readGameHelp 3 xs
            in  case game of
                    Nothing -> Nothing
                    Just g -> Just $ g {p2Name = tail value}
          readGameHelp 5 ((key, value):xs) = 
            let game = readGameHelp 4 xs
            in  case game of
                    Nothing -> Nothing
                    Just g -> Just $ g {p1Name = tail value}                        

showGame :: Game -> String
showGame game = 
    "Player1:" ++ (p1Name game) ++ ";" ++ "Player2:" ++ (p2Name game) ++ ";" ++ "P1Hands:" ++ (show (playerOne game)) ++ ";" ++ "P2Hands:" ++ (show (playerTwo game)) ++ ";" ++ "CurrentTurn:" ++ (show (turn game)) ++ ";" ++ "TurnCount:" ++ (show (turnCount game))
--showGame takes a game and provides a string that describes the game fully and is reversible to create a game if needed
--Ex: showGame game = "Player1:Ashwin;Player2:Josh;P1Hands:[1,1,1,1];P2Hands:[1,1,1,1];CurrentTurn:PlayerOne;TurnCount:50"
                
writeGame :: Game -> FilePath -> IO ()
writeGame game fileName =
    let
        gameString = showGame game
    in
        writeFile fileName gameString

loadGame :: FilePath -> IO Game
loadGame fileName = 
    do
        gameString <- readFile fileName
        return (fromJust (readGame gameString)) 

putBestMove :: Game -> IO ()
putBestMove game = 
    do
        putStrLn (show(bestMove game))