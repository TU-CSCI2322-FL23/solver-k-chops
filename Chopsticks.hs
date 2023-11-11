import Data.Maybe

type Hand = Int
type Winner = Player

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


--Player Functions

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
makeMove game (Add aHand dHand) = -- make both makeMove definitions check if move is valid and return Nothing if so before computing effect?
    do  (attackerHands, defenderHands) <- handsFor game
        attackerHand <- getHand attackerHands aHand  --choose an index in [1,1,1,1,1] so attacker hand should = 1
        defenderHand <- getHand defenderHands dHand --choose an index in [1,1,1,1,1] so defender hand should = 1
        sumFingers <- Just $ attackerHand + defenderHand --add attacker hand too defender hand
        overflow <- Just $ sumFingers `mod` 5  --overflow is the remainder of sumFingers / 5
        updateDefenderHand <- updateHand defenderHands dHand overflow --update defender hand with overflow, uses helper updateHand to save at correct index
        updateSide game (opponent $ turn game) updateDefenderHand
makeMove game Split =  --this takes the TOTAL number of fingers accross all hands, and divides them evenly[5,4,3] -> [4,4,4]
    do  (attackerHands, defenderHands) <- handsFor game
        split <- Just $ fromIntegral (sum attackerHands) `div` fromIntegral (length attackerHands)
        updatedAttackerHand <- Just $ replicate (length attackerHands) split
        updateSide game (turn game) updatedAttackerHand

handsFor :: Game -> Maybe ([Hand], [Hand])
handsFor game = 
    case turn game of 
        PlayerOne -> Just (playerOne game, playerTwo game)
        PlayerTwo -> Just (playerTwo game, playerOne game) 

getHand :: [Hand] -> Int -> Maybe Hand
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

updateHand :: [Hand] -> Int -> Int -> Maybe [Hand] --this is a helper function that makeMove uses to "place" the NEW value at the given index.
updateHand [] index newValue = Nothing 
updateHand (x:xs) 0 0 = Just xs
updateHand (x:xs) 0 newValue = Just $ newValue:xs
updateHand (x:xs) index newValue = consMaybeList x (updateHand xs (index - 1) newValue)
    where consMaybeList :: Hand -> Maybe [Hand] -> Maybe [Hand]
          consMaybeList _ Nothing = Nothing
          consMaybeList h (Just lst) = Just (h:lst)

updateSide :: Game -> Player -> [Hand] -> Maybe Game
updateSide game PlayerOne hand = Just $ game {playerOne = hand, turn = opponent $ turn game, turnCount = (turnCount game) - 1}
updateSide game PlayerTwo hand = Just $ game {playerTwo = hand, turn = opponent $ turn game, turnCount = (turnCount game) - 1}

--GameState Functions

getWinner :: Game -> Maybe Winner
getWinner game = 
    if null $ playerOne game 
        then Just PlayerTwo
    else if null $ playerTwo game 
        then Just PlayerOne
    else Nothing

-- legalMoves :: Game -> [Move]
-- legalMoves game = 
--     if canSplit $ turn game
--         then [Add, Split]
--     else [Add]
--     where canSplit :: Player -> Bool
--           canSplit p = 
--             case p of 
--                 PlayerOne -> canSplitHelp $ playerOne game
--                 PlayerTwo -> canSplitHelp $ playerTwo game
--           canSplitHelp :: [Hand] -> Bool
--           canSplitHelp hands = (sum hands) >= (length hands)

opponent :: Player -> Player
opponent PlayerOne = PlayerTwo
opponent PlayerTwo = PlayerOne

showGame :: Game -> String
showGame game = undefined

prettyShowGame :: Game -> String
prettyShowGame game = undefined