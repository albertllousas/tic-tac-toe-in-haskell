{-# OPTIONS_GHC -Werror=incomplete-patterns #-}
module TicTacToe (
    Player(..), Field(..), Grid,  Status(..), Game(..), TicTacToeError(..), GridPosition, initialGame, play
) where

import Data.List

data Player = X | O deriving (Eq, Show)

data Field = Empty | TakenBy Player deriving (Eq, Show)

type Grid = [[Field]]

data Status = NextTurn Player | Winner Player | GameIsADraw deriving (Eq, Show)

data Game =  Game { grid:: Grid, status :: Status } deriving (Eq, Show)

data TicTacToeError =
    InvalidGridSize |
    GameAlreadyFinished |
    FieldAlreadyTaken |
    InvalidPlayerTurn |
    InvalidGridPosition deriving (Eq, Show)

type GridPosition = (Int,Int) -- (row, column)

initialGame = Game { grid = [ [Empty,Empty,Empty], [Empty,Empty,Empty], [Empty,Empty,Empty] ], status = NextTurn X  }

play :: Player -> GridPosition -> Game -> Either TicTacToeError Game
play player pos game@Game{ grid = g, status = NextTurn s }
        | player /= s = Left InvalidPlayerTurn
        | not (validGame game) = Left InvalidGridSize
        | not (validGridPosition pos) = Left InvalidGridPosition
        | ifThreeInARow newGrid s = Right $ game { grid = newGrid, status = Winner s }
        | ifThreeInAColumn newGrid s = Right $ game { grid = newGrid, status = Winner s }
        | ifThreeInADiagonal newGrid s = Right $ game { grid = newGrid, status = Winner s }
        | ifAllFieldsAreTaken newGrid = Right $ game { grid = newGrid, status = GameIsADraw }
        | otherwise = Right $ game { grid = newGrid, status = NextTurn (next s) }
    where newGrid = markField player pos g
play _ _ _ = Left $ GameAlreadyFinished

next O = X
next X = O

validGame game@Game { grid = [[_,_,_],[_,_,_],[_,_,_]], status=_} = True
validGame _ = False

validGridPosition _ = True

ifThreeInARow g s = (nub (g !! 0)) == [TakenBy s] || (nub (g !! 1)) == [TakenBy s] || (nub (g !! 2)) == [TakenBy s]

ifThreeInAColumn g s = (nub (column 0 g)) == [TakenBy s] || (nub (column 1 g)) == [TakenBy s] || (nub (column 2 g)) == [TakenBy s]
  where column c g = (g!!0!!c):(g!!1!!c):(g!!2!!c):[]

ifThreeInADiagonal g s = nub ((g!!0!!0):(g!!1!!1):(g!!2!!2):[]) == [TakenBy s] || nub ((g!!2!!0):(g!!1!!1):(g!!0!!2):[]) == [TakenBy s]

ifAllFieldsAreTaken g = check (elemIndex Empty (concat g))
  where check (Just _) = False
        check Nothing = True

markField :: Player -> GridPosition-> Grid -> Grid
markField player (i, j) grid = case splitAt i grid of
                             (prevRow, currentRow:afterRow) -> case (splitAt j currentRow) of
                               (prevColumn, _:afterColumn) -> prevRow ++ (prevColumn ++ (TakenBy player): afterColumn): afterRow
                               _ -> grid
                             _ -> grid
