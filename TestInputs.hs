module TestInputs where
import Chopsticks

--g = game
--s = showGame str of g
--(x) = number of moves til end
--c = complex case, not 4s to 1s or 3s to 2s

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
-- legalMoves g3 = [Add 0 0,Add 0 1,Add 0 2,Add 1 0,Add 1 1,Add 1 2]
-- getResult g3 = Nothing
-- whoWillWin g3 = Winner PlayerOne
-- bestMove g3 = Nothing
-- makeMove g3 (Add 1 0) = Just (Game {playerOne = [4,4], playerTwo = [1,1], p1Name = "Henry", p2Name = "Ashwin", turn = PlayerOne, turnCount = 42})
-- readGame s3 == Just g3
-- showGame g3 == s3

--game two moves from end, complex case
gc2 = Game {playerOne = [2], playerTwo = [1], p1Name = "OneHandHarry", p2Name = "OneFingerFrank", turn = PlayerOne, turnCount = 50}
sc2 = "OneHandHarry;OneFingerFrank;2;1;1;50"
-- expected return values...
-- legalMoves gc2 [Add 0 0]
-- getResult gc2 = Nothing
-- whoWillWin gc2 = Winner PlayerTwo
-- bestMove gc2 = Nothing
-- makeMove gc2 (Add 0 0) = Just (Game {playerOne = [2], playerTwo = [3], p1Name = "OneHandHarry", p2Name = "OneFingerFrank", turn = PlayerTwo, turnCount = 49})
-- readGame sc2 == Just gc2
-- showGame gc2 = sc2

--game three moves from end, complex case
gc3 = Game {playerOne = [1], playerTwo = [1], p1Name = "OneHandHarry", p2Name = "OneFingerFrank", turn = PlayerOne, turnCount = 50}
sc3 = "OneHandHarry;OneFingerFrank;1;1;1;50"
-- expected return values...
-- legalMoves gc3 [Add 0 0]
-- getResult gc3 = Nothing
-- whoWillWin gc3 = Winner PlayerOne
-- bestMove gc3 = (Add 0 0)
-- makeMove gc3 (Add 0 0) = Just (Game {playerOne = [1], playerTwo = [2], p1Name = "OneHandHarry", p2Name = "OneFingerFrank", turn = PlayerTwo, turnCount = 49})
-- readGame sc3 == Just gc3
-- showGame gc3 = sc3

--game four moves from end, complex case
--doesn't work, need to make a better case 
gc4 = Game {playerOne = [1,1,1,1,1,1,1,1,1], playerTwo = [4], p1Name = "TwoHandTerry", p2Name = "AmbidexterousAndy", turn = PlayerTwo, turnCount = 20}
sc4 = "TwoHandTerry;AmbidexterousAndy;1,1,1,1,1,1,1,1,1;4;2;20"
-- expected return values...
-- legalMoves gc4 [Add 0 0, Add 0 1, Add 0 2, Add 0 3, Add 0 4, Add 0 5, Add 0 6, Add 0 7, Add 0 8]
-- getResult gc4 = Nothing
-- whoWillWin gc4 = Winner PlayerOne
-- bestMove gc4 = Nothing
-- makeMove gc4 (Add 0 5) = Just (Game {playerOne = [1,1,1,1,1,1,1,1], playerTwo = [4], p1Name = "TwoHandTerry", p2Name = "AmbidexterousAndy", turn = PlayerOne, turnCount = 19})
-- readGame sc4 == Just gc4
-- showGame gc4 = sc4

