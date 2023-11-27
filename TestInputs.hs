import Chopsticks

-- finished game
g0 = Game {playerOne = [1], playerTwo = [], p1Name = "Josh", p2Name = "Ashwin", turn = PlayerTwo, turnCount = 25}
s0 = "Player1:Josh;Player2:Ashwin;P1Hands:[1];P2Hands:[];CurrentTurn:PlayerTwo;TurnCount:25"
-- expected return values...
-- legalMoves g0 = []
-- getResult g0 = Just (Winner PlayerOne)
-- whoWillWin g0 = PlayerOne
-- bestMove g0 = Nothing
-- makeMove 
-- readGame 
-- showGame 

-- game one move from end
g1 = Game {playerOne = [], playerTwo = [], p1Name = "Josh", p2Name = "Ashwin", turn = PlayerOne, turnCount = 50}
s1 = undefined
-- expected return values...
-- legalMoves g1 = 
-- getResult g1 = 
-- whoWillWin g1 = 
-- bestMove g1 = 
-- makeMove 
-- readGame 
-- showGame 

-- game two moves from end
g2 = Game {playerOne = [], playerTwo = [], p1Name = "Josh", p2Name = "Ashwin", turn = PlayerOne, turnCount = 50}
s2 = undefined
-- expected return values...
-- legalMoves g2 = 
-- getResult g2 = 
-- whoWillWin g2 = 
-- bestMove g2 = 
-- makeMove 
-- readGame 
-- showGame 

-- game four moves from end
g4 = Game {playerOne = [], playerTwo = [], p1Name = "Josh", p2Name = "Ashwin", turn = PlayerOne, turnCount = 50}
s4 = undefined
-- expected return values...
-- legalMoves g4 = 
-- getResult g4 = 
-- whoWillWin g4 = 
-- bestMove g4 = 
-- makeMove 
-- readGame 
-- showGame 