# ttt

Tic-tac-toe with unbeatable computer opponent

Updated: March 29 2015 by Robert Johansson

## Usage

To start the game (computer is playing X and human is playing O):

	(ttt (make-game "computer" "human"))

To make the human player start the game (play as X):

	(ttt (make-game "human" "computer"))
 
Print "i" in the game for instructions.

Get debug info with:

	(ttt (debug (make-game "computer" "human")))

