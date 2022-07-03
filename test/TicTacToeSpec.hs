module TicTacToeSpec where

import Test.Hspec
import TicTacToe


spec :: Spec
spec = do

  describe "acceptance" $ do

    it "Play a full game" $ do
      let result = do
                  g <- (play X (1,1) initialGame)
                  g' <- (play O (0,1) g)
                  g'' <- (play X (1,0) g')
                  g''' <- (play O (1,2) g'')
                  g'''' <- (play X (0,0) g''')
                  g''''' <- (play O (2,0) g'''')
                  (play X (2,2) g''''')
      let expectedGrid = [[TakenBy X, TakenBy O, Empty],
                          [TakenBy X, TakenBy X, TakenBy O],
                          [TakenBy O, Empty, TakenBy X]]
      result `shouldBe` (Right $ Game { grid = expectedGrid, status= Winner X })

  describe "game rules" $ do

    it "A game starts with all the fields empty in the grid" $ do
      grid initialGame `shouldBe` [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]

    it "The 'X' player goes always first" $ do
      status initialGame `shouldBe` NextTurn X

    it "A player can take a field if not already taken" $ do
      fmap grid (play X (1,1) initialGame) `shouldBe` (Right $ [[Empty, Empty, Empty], [Empty, TakenBy X , Empty], [Empty, Empty, Empty]])

    it "Players take turns taking fields until the game is over" $ do
      fmap status (play X (1,1) initialGame) `shouldBe` (Right $ NextTurn O)

    it "A game is over when all fields in a row are taken by a player, this player wins the game" $ do
      let ongoingGame = Game {
        grid = [[TakenBy X, TakenBy X, Empty],
                [TakenBy O, TakenBy O, Empty],
                [Empty, Empty, Empty]],
        status = NextTurn X
      }
      fmap status (play X (0,2) ongoingGame) `shouldBe` (Right $ Winner X)

    it "A game is over when all fields in a column are taken by a player, this player wins the game" $ do
      let ongoingGame = Game {
        grid = [[TakenBy X, TakenBy O, Empty],
                [TakenBy X, TakenBy O, Empty],
                [Empty, Empty, Empty]],
        status = NextTurn X
      }
      fmap status (play X (2,0) ongoingGame) `shouldBe` (Right $ Winner X)

    it "A game is over when all fields in a column are taken by a player, this player wins the game" $ do
      let ongoingGame = Game {
        grid = [[TakenBy X, TakenBy O, Empty],
                [TakenBy O, TakenBy X, Empty],
                [Empty, Empty, Empty]],
        status = NextTurn X
      }
      fmap status (play X (2,2) ongoingGame) `shouldBe` (Right $ Winner X)

    it "A game is over when all fields are taken, there is no winner in this case" $ do
      let ongoingGame = Game {
        grid = [[TakenBy X, TakenBy X, TakenBy O],
                [TakenBy X, TakenBy O, TakenBy X],
                [TakenBy O, TakenBy O, Empty]],
        status = NextTurn X
      }
      fmap status (play X (2,2) ongoingGame) `shouldBe` (Right $ GameIsADraw)


  describe "game errors" $ do

    it "should fail when trying to play a wrong field position" $ do
      play X (1,4) initialGame `shouldBe` Left InvalidGridPosition
--
--    it "should fail when a player play to times consecutively" $ do
--      play X (1,1) initialGame `shouldBe` Left InvalidPlayerTurn
--
--    it "should fail when trying to play an already marked field in the grid" $ do
--      play X (1,1) initialGame `shouldBe` Left FieldAlreadyTaken
--
--    it "should fail when trying to play a finished game with a winner" $ do
--      play X (1,1) initialGame `shouldBe` Left GameAlreadyFinished
--
--    it "should fail when trying to play a finished game with a draw" $ do
--      play X (1,1) initialGame `shouldBe` Left GameAlreadyFinished

--it "should not be valid if it has not nine fields in a 3x3 grid" $ do
--      let game = Game { grid = [[Empty, Empty, Empty]], status = NextTurn X }
--      valid game `shouldBe` Left InvalidGridSize

--    describe "Acceptance" $ do

--        it "full game" $ do