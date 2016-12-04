module Play where

import MasterMind
import System.IO
import System.Random
import Text.Read
import Graphics

type TournamentState = (Int,Int)   -- wins, losses

play :: Game -> Result -> TournamentState -> IO TournamentState

play game start tournament_state =
  let (wins, losses) = tournament_state in
   do
      putStrLn ("Tournament results: "++ show wins++ " wins "++show losses++" losses")
      person_play game start tournament_state
      return tournament_state

person_play :: Game -> Result -> TournamentState -> IO TournamentState
person_play game (EndOfGame 0 code boxOfPegs) (wins,losses) =
   do
      putStrLn "Computer won!"
      drawmm code
      play game (game (Start (drop 4 boxOfPegs))) (wins,losses+1)
person_play game (EndOfGame 1 code boxOfPegs) (wins,losses) =
 do
    putStrLn "You won!"
    drawmm code
    play game (game (Start (drop 4 boxOfPegs))) (wins+1,losses)
person_play game (ContinueGame state) tournament_state =
   do
      putStrLn ("Guesses so far: \n"++show state++" Enter your next guess ")
      line <- getLine
      case readMaybe line :: Maybe Code of
        Just x ->  person_play game (game (Move x state)) tournament_state
        Nothing -> putStrLn "Invalid input, try again."
          >> (person_play game (ContinueGame state) tournament_state)

playmm = do
  g <- getStdGen
  boxOfPegs <- return ([n | n <- (randoms g :: [CodePeg])] :: BoxOfPegs)
  person_play mastermind (mastermind (Start boxOfPegs)) (0,0)
