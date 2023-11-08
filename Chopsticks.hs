
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



data Move = Add | Split deriving (Show, Eq)
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

-- A player is a hand a hand is a list of ints i.e [1,1,1,1,1,1] each 1 is a hand the 1 represents how many fingers are on a hand. OVerflow if >5 go back to 1 so 6 -> 1 7 -> 2 etc.
--make a new list every time the hand is updated

makeMove :: Game -> Move -> (Int, Int) -> [Hand]
makeMove game move (aHand, dHand) = 
    if move == Add
    then 
        let attackerHand = getHand (playerOne game) aHand  --choose an index in [1,1,1,1,1] so attacker hand should = 1
            defenderHand = getHand (playerTwo game) dHand --choose an index in [1,1,1,1,1] so defender hand should = 1
            sumFingers = attackerHand + defenderHand --add attacker hand too defender hand
            overflow = sumFingers `mod` 5  --overflow is the remainder of sumFingers / 5
            updateDefenderHand = updateHand (playerTwo game) dHand overflow --update defender hand with overflow 
        in updateDefenderHand
    else updatedAttackerHand
        where split = fromIntegral (sum (playerOne game)) `div` fromIntegral (length (playerOne game))
              updatedAttackerHand = replicate (length (playerOne game)) split


updateHand :: [Hand] -> Int -> Int -> [Hand]
updateHand [] index newValue = error "No Hand Found"
updateHand [x] index newValue = [newValue]
updateHand (x:xs) index newValue = if index == 0 then newValue:xs else x:updateHand xs (index - 1) newValue

--getHand :: [Hand] -> Int -> Hand
--getHand [] index = error "No Hand Found"
--getHand [x] index = x
--getHand (x:xs) index =
--    if index == 0
   --     then
     --       x
    --else
      --  getHand xs (index - 1)