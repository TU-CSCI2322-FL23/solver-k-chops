module TestInputs where
import Chopsticks

-- finished game
g0 = Game {playerOne = [1], playerTwo = [], p1Name = "Josh", p2Name = "Ashwin", turn = PlayerTwo, turnCount = 25}
s0 = "Josh;Ashwin;1;;2;25"
-- expected return values...
-- legalMoves g0 = []
-- getResult g0 = Just (Winner PlayerOne)
-- whoWillWin g0 = Winner PlayerOne
-- bestMove g0 = Nothing
-- makeMove g0 (Add 0 0) = Nothing
-- readGame s0 == Just g0 
-- showGame g0 == s0

-- game one move from end
g1 = Game {playerOne = [3], playerTwo = [2,2,2], p1Name = "Henry", p2Name = "Seven", turn = PlayerTwo, turnCount = 10}
s1 = "Henry;Seven;3;2,2,2;2;10"
-- expected return values...
-- legalMoves g1 = [Add 0 0,Add 1 0,Add 2 0]
-- getResult g1 = Nothing
-- whoWillWin g1 = Winner PlayerTwo
-- bestMove g1 = Just (Add 0 0)
-- makeMove g1 (Add 0 0) = Just (Game {playerOne = [], playerTwo = [2,2,2], p1Name = "Henry", p2Name = "Seven", turn = PlayerOne, turnCount = 9})
-- readGame s1 == Just g1
-- showGame g1 == s1

-- game two moves from end
g2 = Game {playerOne = [3], playerTwo = [2,2,2], p1Name = "James", p2Name = "Josh", turn = PlayerOne, turnCount = 31}
s2 = "James;Josh;3;2,2,2;1;31"
-- expected return values...
-- legalMoves g2 = [Add 0 0,Add 0 1,Add 0 2]
-- getResult g2 = Nothing
-- whoWillWin g2 = Winner PlayerTwo
-- bestMove g2 = Nothing
-- makeMove g2 (Add 0 1) = Just (Game {playerOne = [3], playerTwo = [2,2], p1Name = "James", p2Name = "Josh", turn = PlayerTwo, turnCount = 30})
-- readGame s2 == Just g2
-- showGame g2 == s2

-- game four moves from end
g4 = Game {playerOne = [4,4,4], playerTwo = [1,1], p1Name = "Henry", p2Name = "Ashwin", turn = PlayerTwo, turnCount = 43}
s4 = "Henry;Ashwin;4,4,4;1,1;2;43"
-- expected return values...
-- legalMoves g4 = [Add 0 0,Add 0 1,Add 0 2,Add 1 0,Add 1 1,Add 1 2]
-- getResult g4 = Nothing
-- whoWillWin g4 = Winner PlayerOne
-- bestMove g4 = Nothing
-- makeMove g4 (Add 1 0) = Just (Game {playerOne = [4,4], playerTwo = [1,1], p1Name = "Henry", p2Name = "Ashwin", turn = PlayerOne, turnCount = 42})
-- readGame s4 == Just g4
-- showGame g4 == s4