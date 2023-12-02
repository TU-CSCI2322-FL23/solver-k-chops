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
} deriving (Show, Eq)

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


makeMove :: Game -> Move -> Maybe Game
makeMove game move = 
    if (move `elem` (legalMoves game))
        then let (attackerHands, defenderHands) = handsFor game
             in case move of 
                 (Add aHand dHand) -> 
                     do  updateDefenderHand <- addHands attackerHands defenderHands aHand dHand --update defender hand with overflow, uses helper updateHand to save at correct index
                         updateSide game (opponent $ turn game) updateDefenderHand
                 Split -> --this takes the TOTAL number of fingers accross all hands, and divides them evenly[5,4,3] -> [4,4,4]
                     do  let split = fromIntegral (sum attackerHands) `div` fromIntegral (length attackerHands)
                         let updatedAttackerHand = replicate (length attackerHands) split
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
    case (getResult game) of
        Just result -> []
        Nothing -> 
            if (((sum pHand) `mod` (length pHand) == 0) && (not $ all (\h -> h == ((sum pHand) `div` (length pHand))) pHand))
                then Split : allAdds
            else allAdds
            where pHand = if (turn game == PlayerOne) then playerOne game else playerTwo game
                  allAdds :: [Move]
                  allAdds = 
                    let numP1Hands = length $ playerOne game
                        numP2Hands = length $ playerTwo game
                        p1Indices = [0..numP1Hands-1]
                        p2Indices = [0..numP2Hands-1]
                        crossProd = 
                            case (turn game) of 
                                PlayerOne -> [(x, y) | x <- p1Indices, y <- p2Indices]
                                PlayerTwo -> [(x, y) | x <- p2Indices, y <- p1Indices]
                    in [Add x y | (x, y) <- crossProd]
-- splitting is legal when the number of fingers can be divided evenly among the current player's remaining hands, as long as doing so would actually modify their hands
-- an Add move is legal if both Ints provided are valid indices into the two players' hands; any existing hand can attack any existing hand of its opponent
-- returns an empty list if the game has ended

opponent :: Player -> Player
opponent PlayerOne = PlayerTwo
opponent PlayerTwo = PlayerOne

prettyShowGame :: Game -> IO ()
prettyShowGame game = putStrLn ((p1Name game) ++ " -> " ++ hand1 ++ "\n --------------- \n" ++ (p2Name game) ++ " -> " ++ hand2 ++ "\n" ++ turnName ++ "'s turn! " ++ (show $ turnCount game) ++ " turns left!")
    where hand1 :: String
          hand1 = intercalate " " (map show (playerOne game))
          hand2 :: String
          hand2 = intercalate " " (map show (playerTwo game))
          turnName :: String
          turnName = if ((turn game) == PlayerOne) then p1Name game else p2Name game
