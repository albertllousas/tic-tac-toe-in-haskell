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
