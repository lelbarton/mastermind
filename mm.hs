type Code = (CodePeg, CodePeg, CodePeg, CodePeg)  -- a player's guess of four CodePegs
type State = (Code, Code, [Guess]) -- the code to be guessed, the next player input, previous guesses

data CodePeg = Yellow | Red | Blue | Purple | Green | Orange -- available CodePeg
data KeyPeg = W | B | O -- white, black, and no peg
type Hint = (KeyPeg, KeyPeg, KeyPeg, KeyPeg)
-- a player's guess and the response
type Guess = (Code, Hint)
