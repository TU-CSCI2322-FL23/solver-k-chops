
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
initalizeGame playerOneName playerTwoName kHands = Game {
    playerOne = Player {hands = replicate kHands 1, name = playerOneName  },
    playerTwo = Player {hands = replicate kHands 1, name = playerTwoName  },
    currentPlayer = playerOne
}
--found this stackoverflow link for the function replicate https://stackoverflow.com/questions/22101589/create-a-list-of-specified-length-in-haskell