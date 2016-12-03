module Play where

import MasterMind
import System.IO
import System.Random

type TournammentState = (Int,Int)   -- wins, losses


play :: Game -> Result -> TournammentState -> IO TournammentState

play game start tournament_state =
  let (wins, losses) = tournament_state in
   do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses")
      person_play game start tournament_state
      return tournament_state

person_play :: Game -> Result -> TournammentState -> IO TournammentState
person_play game (EndOfGame 0 boxOfPegs) (wins,losses) =
   do
      putStrLn "Computer won!"
      play game (game (Start (drop 4 boxOfPegs))) (wins,losses+1)
person_play game (EndOfGame 1 boxOfPegs) (wins,losses) =
 do
    putStrLn "You won!"
    play game (game (Start (drop 4 boxOfPegs))) (wins+1,losses)
person_play game (ContinueGame state) tournament_state =
   do
      putStrLn ("Guesses so far: \n"++show state++" Enter your next guess ")
      line <- getLine
      person_play game (game (Move (read line :: Code) state)) tournament_state


playMM = do
  g <- getStdGen  
  boxOfPegs <- return ([n | n <- (randoms g :: [CodePeg])] :: BoxOfPegs)
  person_play mastermind (mastermind (Start boxOfPegs)) (0,0)


--boxOfPegs = [Yellow,Blue,Red,Orange,Red,Red,Yellow,Blue]
