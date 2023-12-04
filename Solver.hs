module Solver where
import Chopsticks
import Data.Maybe

gameOutcome :: Game -> [Game]
gameOutcome game = 
        let
            allMoves = legalMoves game
        in    
            mapMaybe (\move -> makeMove game move) allMoves
            
whoWillWin :: Game ->  Result
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