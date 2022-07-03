# tic-tac-toe-in-haskell

The rules of the tic-tac-toe game are the following:

- A game has nine fields in a 3x3 grid
- There are two player in the game ("X" and "O")
- The "X" player goes always first
- A player can take a field if not already taken
- Players take turns taking fields until the game is over
- A game is over when all fields in a row are taken by a player, this player wins the game  
- A game is over when all fields in a column are taken by a player, this player wins the game
- A game is over when all fields in a diagonal are taken by a player, this player wins the game  
- A game is over when all fields are taken, there is no winner in this case

## Solution

The solution is just one function:

```haskell
play :: Player -> GridPosition -> Game -> Either TicTacToeError Game
```
Where grid:
```shell
| (0,0) (0,1) (0,2) |
| (1,0) (1,1) (1,2) |
| (2,0) (2,1) (2,2) |
```

Take a look on the [tests](/test/TicTacToeSpec.hs) to see how to see it in action!

## Running tests

```shell
stack test

TicTacToe
  acceptance
    Play a full game
  game rules
    A game starts with all the fields empty in the grid
    The 'X' player goes always first
    A player can take a field if not already taken
    Players take turns taking fields until the game is over
    A game is over when all fields in a row are taken by a player, this player wins the game
    A game is over when all fields in a column are taken by a player, this player wins the game
    A game is over when all fields in a column are taken by a player, this player wins the game
    A game is over when all fields are taken, there is no winner in this case
  game errors
    should fail when trying to play a wrong field position
    should fail when a player plays in the wrong turn 
    should fail when trying to play an already marked field in the grid
    should fail when trying to play a finished game with a winner
    should fail when trying to play a finished game with a draw
    should not be valid if it has not nine fields in a 3x3 grid

Finished in 0.0018 seconds
15 examples, 0 failures
```