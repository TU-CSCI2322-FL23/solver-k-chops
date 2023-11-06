
type Hand = Int
type Winner = Player


data Game = Game { 
    playerOne :: [Hand],
    playerTwo :: [Hand],
    p1Name :: String,
    p2Name :: String,
    turn :: Player
} deriving (Show)

data Player = PlayerOne | PlayerTwo deriving (Show)


-- data Side = Side {
--     hands :: [Hand],
--     name :: String
-- } deriving Show



data Move = Add | Split
    --a move takes in a game and returns a game
    -- these are defining functions, but i don't think we can do that


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
            turn = PlayerOne
        }
--found this stackoverflow link for the function replicate https://stackoverflow.com/questions/22101589/create-a-list-of-specified-length-in-haskell


--Player Functions

getHand :: [Hand] -> Int -> Hand
getHand [] index = error "No Hand Found"
getHand [x] index = x
getHand (x:xs) index =
    if index == 0
        then
            x
    else
        getHand xs (index - 1)

-- getHand shows the number of fingers on that hand
--Ex: getHand playerOne game 5 (where playerOne Hands are [2,1,3,5,4]) = 4 


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

showHand :: Game -> Player -> Int -> String 
showHand game player handIndex
    |currentHand == 0 =  handAsciiArt 0
    |currentHand == 1 =  handAsciiArt 1
    |currentHand == 2 =  handAsciiArt 2
    |currentHand == 3 =  handAsciiArt 3
    |currentHand == 4 =  handAsciiArt 4
    where
        playerHand = case player of
            PlayerOne -> playerOne game
            PlayerTwo -> playerTwo game
        currentHand = getHand playerHand handIndex
--showHand takes a player and their hand index and returns ascii art for it
--Ex: showHand PlayerOne 2 where playerOne's hands are [1,2,3,4,0] ==  "---'____) \n" ++
--                                                                        "      _________) \n" ++
--                                                                        "      ___________) \n" ++
--                                                                        "      (____) \n" ++
--                                                                        "---.(_____)


--Move Functions


--Other Functions

add :: Game -> Player -> Player -> Player
add game attacker defender = updatedPlayer
    where
        updatedPlayer = undefined --choose index on the attacker and add that index to another chosen index of the defender if the defenders hand ends up greater than 5 we overflow
        
    


split :: Game -> [Hand] -> Game
split = undefined