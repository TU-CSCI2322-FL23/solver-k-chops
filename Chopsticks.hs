
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

-- getHandHelper :: [Hand] -> Int -> Hand
-- getHandHelper [] index = error "No Hand Found"
-- getHandHelper [x] index = x
-- getHandHelper (x:xs) index =
--     if index == 0
--         then
--             x
--     else
--         getHandHelper xs (index - 1)


-- getHand :: Player -> Int -> Hand
-- getHand player handNumber = getHandHelper (hands player) handNumber
--getHand shows the number of fingers on that hand
--Ex: getHand playerOne 5 where playerOne Hands are [2,1,3,5,4] = 4 


-- handAsciiArt :: Int -> String
-- handAsciiArt 0 = "---'____) \n" ++
--               "      (_____) \n" ++
--               "      (_____) \n" ++
--               "      (____) \n" ++
--               "---.__(___) \n"


-- handAsciiArt 1 = "---'____) \n" ++
--               "      _________) \n" ++
--               "      (_____) \n" ++
--               "      (____) \n" ++
--               "---.__(___) \n"


-- handAsciiArt 2 = "---'____) \n" ++
--               "      _________) \n" ++
--               "      ___________) \n" ++
--               "      (____) \n" ++
--               "---.(_____) \n"


-- handAsciiArt 3 ="---'____) \n" ++
--               "      _________) \n" ++
--               "      ___________) \n" ++
--               "      _________) \n" ++
--               "---.(_____) \n"


-- handAsciiArt 4 = "---'____) \n" ++
--               "      _________) \n" ++
--               "      ___________) \n" ++
--               "      _________) \n" ++
--               "---.________) \n"        

-- showHand :: Player -> Int -> String 
-- showHand player handIndex
--     |(getHand player handIndex) == 0 =  handAsciiArt 0
--     |(getHand player handIndex) == 1 =  handAsciiArt 1
--     |(getHand player handIndex) == 2 =  handAsciiArt 2
--     |(getHand player handIndex) == 3 =  handAsciiArt 3
--     |(getHand player handIndex) == 4 =  handAsciiArt 4
--showHand takes a player and their hand index and returns ascii art for it
--Ex: showHand playerOne 2 where playerOne's hands are [1,2,3,4,0] ==  "---'____) \n" ++
--                                                                        "      _________) \n" ++
--                                                                        "      ___________) \n" ++
--                                                                        "      (____) \n" ++
--                                                                        "---.(_____)


--Move Functions


--Other Functions