module Chopsticks where
import Data.Maybe
import Data.List
import Data.List.Split

type Hand = Int
data Result = Winner Player | Tie deriving (Show, Eq)

data Game = Game { 
    playerOne :: [Hand],
    playerTwo :: [Hand],
    p1Name :: String,
    p2Name :: String,
    turn :: Player,
    turnCount :: Int
} deriving (Show)

data Player = PlayerOne | PlayerTwo deriving (Show, Eq)

data Move = Add Int Int | Split deriving (Show, Eq)

initializeGame :: String -> String -> Int -> Game 
initializeGame playerOneName playerTwoName kHands = 
    let 
        hands = replicate kHands 1
    in
        Game {
            playerOne = hands,
            playerTwo = hands,
            p1Name = playerOneName,
            p2Name = playerTwoName,
            turn = PlayerOne,
            turnCount = 50
        }


--Pretty Print Helper Functions

handAsciiArt :: Int -> String
handAsciiArt 0 = "---'____) \n" ++
              "      (_____) \n" ++
              "      (_____) \n" ++
              "      (____) \n" ++
              "---.__(___) \n"


handAsciiArt 1 = "---'____) \n" ++
              "      _________) \n" ++
              "      (_____) \n" ++
              "      (____) \n" ++
              "---.__(___) \n"


handAsciiArt 2 = "---'____) \n" ++
              "      _________) \n" ++
              "      ___________) \n" ++
              "      (____) \n" ++
              "---.(_____) \n"


handAsciiArt 3 ="---'____) \n" ++
              "      _________) \n" ++
              "      ___________) \n" ++
              "      _________) \n" ++
              "---.(_____) \n"


handAsciiArt 4 = "---'____) \n" ++
              "      _________) \n" ++
              "      ___________) \n" ++
              "      _________) \n" ++
              "---.________) \n"        

-- showHand :: Game -> Player -> Int -> String 
-- showHand game player handIndex
--     |currentHand == 0 =  handAsciiArt 0
--     |currentHand == 1 =  handAsciiArt 1
--     |currentHand == 2 =  handAsciiArt 2
--     |currentHand == 3 =  handAsciiArt 3
--     |currentHand == 4 =  handAsciiArt 4
--     where
--         playerHand = case player of
--             PlayerOne -> playerOne game
--             PlayerTwo -> playerTwo game
--         currentHand = getHand playerHand handIndex

--showHand takes a player and their hand index and returns ascii art for it
--Ex: showHand PlayerOne 2 where playerOne's hands are [1,2,3,4,0] ==  "---'____) \n" ++
--                                                                        "      _________) \n" ++
--                                                                        "      ___________) \n" ++
--                                                                        "      (____) \n" ++
--                                                                        "---.(_____)


--IO Functions

askMove :: IO Move
askMove = do
    putStrLn "Would you like to Add (1) or Split (2)?"
    option <- getLine
    case option of
        -- "1" -> return Add  --this needs to be adjusted to account for updated Add type
        "2" -> return Split
        _ -> do 
                putStrLn "Wrong input"
                askMove --ask Dr. Fogarty if I can also print something along with doing askMove
--askMove either returns Add or Move depending on player input
--Ex: move <- askMove would make move = Add or Split depending on what the player put

--used this line https://www.haskell.org/tutorial/io.html to help write this function (figuring out how to return a type with IO)


chooseHand :: IO (Int,Int)
chooseHand = do
    putStrLn "If attacking, choose an attacking hand and a target hand, if splitting choose a hand to split and a hand to add\n Ex: 3 4"
    output <- getLine
    let outputList = words output 
    case outputList of
        [a,b] -> return (read a, read b) --unsafe. If a and b aren't numbers, then it'll error
        _ -> do 
                putStrLn "Wrong input"
                chooseHand


--Move-Making Functions

-- A player is a hand a hand is a list of ints i.e [1,1,1,1,1,1] each 1 is a hand the 1 represents how many fingers are on a hand. OVerflow if >5 go back to 1 so 6 -> 1 7 -> 2 etc.
--make a new list every time the hand is updated

makeMove :: Game -> Move -> Maybe Game
makeMove game move = 
    if (move `elem` (legalMoves game))
        then let (attackerHands, defenderHands) = handsFor game
             in case move of 
                 (Add aHand dHand) -> 
                     do  updateDefenderHand <- addHands attackerHands defenderHands aHand dHand --update defender hand with overflow, uses helper updateHand to save at correct index
                         updateSide game (opponent $ turn game) updateDefenderHand
                 Split -> --this takes the TOTAL number of fingers accross all hands, and divides them evenly[5,4,3] -> [4,4,4]
                     do  split <- Just $ fromIntegral (sum attackerHands) `div` fromIntegral (length attackerHands)
                         updatedAttackerHand <- Just $ replicate (length attackerHands) split
                         updateSide game (turn game) updatedAttackerHand
    else Nothing

handsFor :: Game -> ([Hand], [Hand])
handsFor game = 
    case turn game of 
        PlayerOne -> (playerOne game, playerTwo game)
        PlayerTwo -> (playerTwo game, playerOne game) 

addHands :: [Hand] -> [Hand] -> Int -> Int -> Maybe [Hand]
addHands attackerHands defenderHands aHand dHand = 
    do  attackerHand <- getHand attackerHands aHand  --choose an index in [1,1,1,1,1] so attacker hand should = 1
        defenderHand <- getHand defenderHands dHand --choose an index in [1,1,1,1,1] so defender hand should = 1
        sumFingers <- Just $ attackerHand + defenderHand --add attacker hand too defender hand
        overflow <- Just $ sumFingers `mod` 5  --overflow is the remainder of sumFingers / 5
        updateHand defenderHands dHand overflow --update defender hand with overflow, uses helper updateHand to save at correct index
    where getHand :: [Hand] -> Int -> Maybe Hand
          getHand [] index = Nothing
          getHand [x] index = 
            if index == 0 
                then Just x
            else Nothing
          getHand (x:xs) index =
            if index == 0
                then Just x
            else getHand xs (index - 1)
          -- getHand shows the number of fingers on that hand
          --Ex: getHand playerOne game 5 (where playerOne Hands are [2,1,3,5,4]) = 4

          updateHand :: [Hand] -> Int -> Int -> Maybe [Hand]
          updateHand [] index newValue = Nothing 
          updateHand (x:xs) 0 0 = Just xs
          updateHand (x:xs) 0 newValue = Just $ newValue:xs
          updateHand (x:xs) index newValue = consMaybeList x (updateHand xs (index - 1) newValue)
            where consMaybeList :: Hand -> Maybe [Hand] -> Maybe [Hand]
                  consMaybeList _ Nothing = Nothing
                  consMaybeList h (Just lst) = Just (h:lst)
          --this is a helper function that addHands uses to "place" the NEW value at the given index.

updateSide :: Game -> Player -> [Hand] -> Maybe Game
updateSide game PlayerOne hand = Just $ game {playerOne = hand, turn = opponent $ turn game, turnCount = (turnCount game) - 1}
updateSide game PlayerTwo hand = Just $ game {playerTwo = hand, turn = opponent $ turn game, turnCount = (turnCount game) - 1}

--GameState Functions

getResult :: Game -> Maybe Result
getResult game 
    | (turnCount game) == 0 = Just Tie
    | null $ (playerOne game) = Just (Winner PlayerTwo)
    | null $ (playerTwo game) = Just (Winner PlayerOne)
    | otherwise = Nothing
    


legalMoves :: Game -> [Move]
legalMoves game = 
    if ((sum pHand) `mod` (length pHand) == 0)
        then Split : allAdds
    else allAdds
    where pHand = if (turn game == PlayerOne) then playerOne game else playerTwo game
          allAdds :: [Move]
          allAdds = 
            let numP1Hands = length $ playerOne game
                numP2Hands = length $ playerTwo game
                p1Indices = [0..numP1Hands-1]
                p2Indices = [0..numP2Hands-1]
                crossProd = [(x, y) | x <- p1Indices, y <- p2Indices]
            in [Add (fst prod) (snd prod)| prod <- crossProd]
-- splitting is legal when the number of fingers can be divided evenly among the remaining hands
-- an Add move is legal if both Ints provided are valid indices into the two players' hands; any existing hand can attack any existing hand of its opponent

opponent :: Player -> Player
opponent PlayerOne = PlayerTwo
opponent PlayerTwo = PlayerOne

showGame :: Game -> IO ()
showGame game = putStrLn ((p1Name game) ++ " -> " ++ hand1 ++ "\n --------------- \n" ++ (p2Name game) ++ " -> " ++ hand2 ++ "\n" ++ turnName ++ "'s turn! " ++ (show $ turnCount game) ++ " turns left!")
    where hand1 :: String
          hand1 = intercalate " " (map show (playerOne game))
          hand2 :: String
          hand2 = intercalate " " (map show (playerTwo game))
          turnName :: String
          turnName = if ((turn game) == PlayerOne) then p1Name game else p2Name game

prettyShowGame :: Game -> String
prettyShowGame game = undefined

describeGame :: Game -> String
describeGame game = 
    "Player1:" ++ (p1Name game) ++ "," ++ "Player2:" ++ (p2Name game) ++ "," ++ "P1Hands:" ++ (show (playerOne game)) ++ "," ++ "P2Hands:" ++ (show (playerTwo game)) ++ "," ++ "CurrentTurn:" ++ (show (turn game)) ++ "," ++ "TurnCount:" ++ (show (turnCount game))
--Describe Game takes a game and provides a string that describes the game fully and is reversible to create a game if needed
--Ex: describeGame game = "Player1:Ashwin,Player2:DickMan,P1Hands:[1,1,1,1],P2Hands:[1,1,1,1],CurrentTurn:PlayerOne,TurnCount:50"    


-- isFutureWinner :: Game -> Maybe Result
-- isFutureWinner game =
--     case (getResult game) of
--         Just result -> Just result
--         Nothing ->
--                 if (turnCount game) == 0
--                     then
--                         Just Tie
--                 else
                     
--                         if nextPlayer == turn game
--                             then 
--                                 if True `elem` (checkWinnerOutcome game recurResults)
--                                     then Just (Winner (turn game))
--                                 else 
--                                     Nothing
--                        else 
--                             if True `elem` (checkWinnerOutcome game recurResults)
--                                 then Just (Winner (turn game))
--                         else Nothing
--     where 
--         checkWinnerOutcome game recurResults = map (\x -> x == Just (Winner (turn game))) recurResults

gameOutcome :: Game -> [Game]
gameOutcome game = 
        let
            allMoves = legalMoves game
        in    
            mapMaybe (\move -> makeMove game move) allMoves
            

whoWillWin :: [Game] -> Maybe Result
whoWillWin [] = Nothing
whoWillWin (game:xs) = 
    if (turnCount game) == 0
        then
            Just Tie
    else
        case (getResult game) of
            Just result -> Just result --this takes care of the case if player already one
            Nothing ->
                let 
                    newGames = (gameOutcome game)
                    recurResults = map (\x -> getResult x) newGames
                    nextPlayer = opponent (turn game)
                in 
                if nextPlayer == (turn game)   --this takes care of situations where the player will win in one turn
                    then 
                        if True `elem` (checkWinnerOutcome game recurResults)
                            then Just (Winner (turn game))
                        else 
                            whoWillWin (gameOutcome game)
                else 
                    if True `elem` (checkWinnerOutcome game recurResults) --I need help with this logic
                        then Just (Winner (turn game))
                    else whoWillWin (gameOutcome game)
    where 
        checkWinnerOutcome game recurResults = map (\x -> x == Just (Winner (turn game))) recurResults
    -- let futureWinner = isFutureWinner x
    -- in
    -- case futureWinner of
    --     Just result -> Just result
    --     Nothing ->
    --         let 
    --             newGameState = gameOutcome (head xs)
    --         in
    --             (whoWillWin newGameState)


