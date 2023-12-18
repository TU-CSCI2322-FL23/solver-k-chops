# game-solver-template

# Project Grade:         84/100
## Functionality               60/73
* Game mechanics:              20
* Exact game solver:           15
* Cut-off depth solver:        10/13
  * You don't check if the game is over, and so crash when the game is finished!
* Evaluation function:         1/2
  * You don't rate won games nearly high enough, and instead just add something for winning.
  * You also score the current player as always 1.
* Avoiding unnecessary work:   0/3
* Command-line interface:      7/10
  * You read and write the raw Haskell values. This is particularly icky for the move flag. Some
    poor behavior on the command line due to other bugs.
* Move and verbose flags:      3/5
  * move flag works, although the output is wrong. Verbose doesn't seem to do anything.
* Error-handling:              4/5
  * Good everywhere except verifying and reading flags 

## Design                      24/27
* Well-designed data types:    8
* Well-decomposed functions:   10
  * some functions that are never used (gameOutcome) laying around.
  * main and legalMoves hav gone down the pyramid of DOOOOM. Some of that can be fixed by moving the 'where'
    in legalMoves back to the right place, or just making allAdds the last variable in a let before the case. 
    For main I would make a helper function with guards to call after the case expression. 
* Good module decomposition:   2
* Good variable names:         2
  * Excellent, honestly.
* Efficient/idiomatic code:    5
  * some do blocks would have made your updateHand much nicer, but it's an ugly problem to solve any
    way you go.
  * You have random newlines all over the place that just make your code so much longer to read.
    WhoWillWin is 20% longer than it needs to be.
