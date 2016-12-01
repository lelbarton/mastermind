module Play where

import MasterMind
import System.IO

type TournammentState = (Int,Int)   -- wins, losses


play :: Game -> Result -> TournammentState -> IO TournammentState

play game start tournament_state =
  let (wins, losses) = tournament_state in
   do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses")
      person_play game start tournament_state
      return tournament_state



person_play :: Game -> Result -> TournammentState -> IO TournammentState
person_play game (EndOfGame 0) (wins,losses) =
   do
      putStrLn "Computer won!"
      play game (game Start) (wins,losses+1)
person_play game (EndOfGame 1) (wins,losses) =
 do
    putStrLn "You won!"
    play game (game Start) (wins+1,losses)
person_play game (ContinueGame state) tournament_state =
   do
      putStrLn ("State is "++show state++" Enter your next guess ")
      line <- getLine
      person_play game (game (Move (read line :: Code) state)) tournament_state
