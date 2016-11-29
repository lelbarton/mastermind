module MasterMind where
import Data.List

-- Data --
data CodePeg = Yellow | Red | Blue | Purple | Green | Orange -- available CodePeg
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

data KeyPeg = W | B | O -- white, black, and no peg
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

type Code = (CodePeg, CodePeg, CodePeg, CodePeg)  -- a player's guess of four CodePegs
type Hint = (KeyPeg, KeyPeg, KeyPeg, KeyPeg) -- a player's score
type Guess = (Code, Hint) -- a player's guess and the response

type State = (Code, [Guess]) -- the code to be guessed, previous guesses

data Action = Move Code State   -- do AMove in State
            | Start              -- returns starting state
            
data Result = EndOfGame Int        -- end of game
            | ContinueGame State   -- continue with new state
         deriving (Eq, Show)     

type Game = Action -> Result
type Player = Game -> Result -> Code

-- MasterMind --
mastermind :: Game
mastermind (Move guess (code, prevresult))
  | guess == code = EndOfGame 1
  | length prevresult == 8 = EndOfGame 0
  | otherwise =
      ContinueGame (code, (guess, (makehint guess code)):prevresult)



-- Makehint + helpers --
makehint :: Code -> Code -> Hint
makehint guess code = (hintnum2tuple numblack numwhite)
  where
    guessl = (\(a,b,c,d) -> [a,b,c,d]) guess
    codel = (\(a,b,c,d) -> [a,b,c,d]) code
    numblack = countblack guessl codel    
    numwhite = countwhite guessl codel

-- first count total number of pegs that are the same between guess and code
countdup [] _ = 0
countdup (l1h:l1t) l2
  | l1h `elem` l2 = 1+(countdup l1t (delete l1h l2))
  | otherwise = countdup l1t l2
-- number of black pegs equal to number of exact matches (position and colour)
countblack l1 l2 = foldr (\(x,y) -> \n -> (if x==y then 1 else 0)+n) 0 (zip l1 l2)
-- number of white pegs equal to intersection less num black pegs
countwhite l1 l2 = (countdup l1 l2) - (countblack l1 l2)

hintnum2tuple b w = (\[a,b,c,d] -> (a,b,c,d)) (hintnum2list b w 4)

hintnum2list :: Int -> Int -> Int -> [KeyPeg]
hintnum2list x y n
  | n == 0 = []
  | x == 0 && y == 0 = O:(hintnum2list 0 0 (n-1))
  | x == 0 = W:(hintnum2list 0 (y-1) (n-1))
  | otherwise = B:(hintnum2list (x-1) y (n-1))


-- guess = (Yellow,Blue,Green,Orange)
-- code = (Yellow,Green,Purple,Blue)
