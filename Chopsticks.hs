
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
            turn = PlayerOne,
            turnCount = 50
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


--IO Functions

askMove :: IO Move
askMove = do
    putStrLn "Would you like to Add (1) or Split (2)?"
    option <- getLine
    case option of
        "1" -> return Add
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



--Other Functions

-- A player is a hand a hand is a list of ints i.e [1,1,1,1,1,1] each 1 is a hand the 1 represents how many fingers are on a hand. OVerflow if >5 go back to 1 so 6 -> 1 7 -> 2 etc.
--make a new list every time the hand is updated

makeMove :: Game -> Move -> (Int, Int) -> Game
makeMove game Add (aHand, dHand) = 
    let (attackerHands, defenderHands) = handsFor game
        attackerHand = getHand attackerHands aHand  --choose an index in [1,1,1,1,1] so attacker hand should = 1
        defenderHand = getHand defenderHands dHand --choose an index in [1,1,1,1,1] so defender hand should = 1
        sumFingers = attackerHand + defenderHand --add attacker hand too defender hand
        overflow = sumFingers `mod` 5  --overflow is the remainder of sumFingers / 5
        updateDefenderHand = updateHand defenderHands dHand overflow --update defender hand with overflow, uses helper updateHand to save ar correct index
    in updateSide game (opponent $ turn game) updateDefenderHand
makeMove game Split (aHand, dHand) = updateSide game (turn game) updatedAttackerHand --I think we will need to change/remove this move entirely, but this takes the TOTAL number of fingers accross all hands, and divides them evenly[5,4,3] -> [4,4,4]
        where (attackerHands, defenderHands) = handsFor game
              split = fromIntegral (sum attackerHands) `div` fromIntegral (length attackerHands)
              updatedAttackerHand = replicate (length attackerHands) split
              playerTurn = turn game 

handsFor :: Game -> ([Hand], [Hand])
handsFor game = 
    case turn game of 
        PlayerOne -> (playerOne game, playerTwo game)
        PlayerTwo -> (playerTwo game, playerOne game) 
               
updateSide :: Game -> Player -> [Hand] -> Game
updateSide game PlayerOne hand = game {playerOne = hand, turn = opponent $ turn game, turnCount = (turnCount game) - 1}
updateSide game PlayerTwo hand = game {playerTwo = hand, turn = opponent $ turn game, turnCount = (turnCount game) - 1}

updateHand :: [Hand] -> Int -> Int -> [Hand] --this is a helper function that makeMove uses to "place" the NEW value at the given index.
updateHand [] index newValue = error "No Hand Found" 
updateHand (x:xs) 0 0 = xs
updateHand (x:xs) 0 newValue = newValue:xs
updateHand (x:xs) index newValue = x:updateHand xs (index - 1) newValue

--GameState Functions
getWinner :: Game -> Maybe Winner
getWinner game = 
    if null $ playerOne game 
        then Just PlayerTwo
    else if null $ playerTwo game 
        then Just PlayerOne
    else Nothing

legalMoves :: Game -> [Move]
legalMoves game = 
    if canSplit $ turn game
        then [Add, Split]
    else [Add]
    where canSplit :: Player -> Bool
          canSplit p = 
            case p of 
                PlayerOne -> canSplitHelp $ playerOne game
                PlayerTwo -> canSplitHelp $ playerTwo game
          canSplitHelp :: [Hand] -> Bool
          canSplitHelp hands = (sum hands) >= (length hands)

opponent PlayerOne = PlayerTwo
opponent PlayerTwo = PlayerOne

showGame :: Game -> String
showGame game = undefined

prettyShowGame :: Game -> String
prettyShowGame game = undefined