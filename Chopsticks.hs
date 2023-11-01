
type Hand = Int
type Winner = Player


data Game = Game { 
    playerOne :: Player,
    playerTwo :: Player,
    currentPlayer :: Player
} deriving (Show)

data Player = Player {
    hands :: [Hand],
    name :: String
    -- decide if keep, isDead :: Bool 
} deriving Show


data Move = Add | Split
    --a move takes in a game and returns a game
    -- these are defining functions, but i don't think we can do that

initalizeGame :: String -> String -> Int -> Game 
initalizeGame playerOneName playerTwoName kHands = 
    let 
        p1 = Player {hands = replicate kHands 1, name = playerOneName  }
        p2 =  Player {hands = replicate kHands 1, name = playerTwoName  }

    in
        Game {
            playerOne = p1,
            playerTwo = p2,
            currentPlayer = p1
    }
--found this stackoverflow link for the function replicate https://stackoverflow.com/questions/22101589/create-a-list-of-specified-length-in-haskell