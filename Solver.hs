module Solver where
import Chopsticks
import Data.Maybe
import Data.List
import Data.Ord
gameOutcome :: Game -> [Game]
gameOutcome game =
        let
            allMoves = legalMoves game
        in
            mapMaybe (\move -> makeMove game move) allMoves

whoWillWin :: Game -> Result
whoWillWin game =
    case (getResult game) of
        Just result -> result --this takes care of the case if player already won
        Nothing ->
            let
                newGames = mapMaybe (makeMove game) (legalMoves game)
                outcomes = map (whoWillWin) newGames
            in
                if (Winner $ turn game) `elem` outcomes --found link to https://zvon.org/other/haskell/Outputprelude/any_f.html 
                    then
                        Winner (turn game)
                else
                    if any (== Tie) outcomes
                        then
                            Tie
                    else
                        Winner (opponent (turn game))

gameMovePair :: Game -> Move -> Maybe (Move, Game)
gameMovePair game move =
    case makeMove game move of
        Just resultGame -> Just (move, resultGame)
        Nothing -> Nothing

bestMove :: Game -> Maybe Move
bestMove game =
        case getResult game of
        Just result -> Nothing
        Nothing ->
            let
                allMoves = legalMoves game
                newGames = mapMaybe (gameMovePair game) allMoves
                outcomes = map (\(move, game) -> (whoWillWin game, move)) newGames
            in
                case lookup (Winner $ turn game) outcomes of
                Just move -> Just move
                Nothing ->
                    case lookup (Tie) outcomes of
                        Just move -> Just move
                        Nothing -> Nothing

handDiff :: [Hand] -> [Hand] -> Int
handDiff playerA playerB = (length playerA) - (length playerB)
--calculates the difference in hands between players. If the resulting Int is negative, than playerA has less hands than playerB. If resulting Int is positive, than playerA has more hands 
--Ex: handDiff (playerOne game) (playerTwo game) where playerOne has [2,3] and playerTwo has [4,5,6] = -1 

-- scoreDist :: [Hand] ->  Int
-- scoreDist player = 
--     let 
--         totalFingers = sum player
--         avgFingerPerHand = totalFingers `div` (length player)
--         dist = [abs (x - avgFingerPerHand) | x <- player]
--         -- scoredDist = map (* (-1)) dist --this basically says that every negative number is positive and every positive number is negative. This promotes players where their hands have less fingers than average. It'll mean players play more defensively 
--     in
--         sum dist

scoreCurrentPlayer :: Game -> Int
scoreCurrentPlayer game =
    if (turn game) == PlayerOne
        then
            1
    else
        1

scoreWinnerLoser :: Game -> Int
scoreWinnerLoser game =
    let result = getResult game
    in
        case result of
            Just (Winner PlayerOne) -> 2
            Just (Winner PlayerTwo) -> -2
            Just Tie -> 1
            Nothing -> 0

rateGame :: Game -> Int
rateGame game =
    let
        p1HandDiff = handDiff (playerOne game) (playerTwo game)
        p1WinScore = scoreWinnerLoser game
        p1CurrentTurn = scoreCurrentPlayer game
    in
        (p1HandDiff * 2) +  p1CurrentTurn + (p1WinScore * 3) 

        
whoMightWin :: Game -> Int -> (Int, Maybe Move)
whoMightWin game depth
    | depth <= 0 = (rateGame game, Nothing)
    | otherwise =
        let
            allMoves = legalMoves game
            newGames = mapMaybe (gameMovePair game) allMoves
            ratedGameList = map (\(move, newGame) -> (fst $ whoMightWin newGame (depth - 1), Just move)) newGames
        in
            if null ratedGameList
                then
                    error "Depth Exceeds Length of Tree"
            else
                if (turn game == PlayerOne)
                    then maximumBy (comparing fst) ratedGameList
                else minimumBy (comparing fst) ratedGameList