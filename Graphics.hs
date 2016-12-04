module Graphics where
import Graphics.Gloss
import MasterMind

window :: Display
window = InWindow "MasterMind" (600, 200) (10, 10) -- floating window with title, width/height, and screen position

background :: Color
background = white -- window background colour

getcolor :: CodePeg -> Color
getcolor peg
    | peg == Yellow = yellow
    | peg == Red = red
    | peg == Blue = blue
    | peg == Purple = mixColors 50 50 red blue
    | peg == Green = green
    | peg == Orange = orange
    | otherwise = black

getfst (a,_,_,_) = a
getsnd (_,a,_,_) = a
getthd (_,_,a,_) = a
getfth (_,_,_,a) = a

render :: Code -> Picture
render code = pictures [
    translate (-225) 0 $ color (getcolor (getfst code)) $ circleSolid 50,
    translate (-75) 0 $ color (getcolor (getsnd code)) $ circleSolid 50,
    translate 75 0 $ color (getcolor (getthd code)) $ circleSolid 50,
    translate 225 0 $ color (getcolor (getfth code)) $ circleSolid 50
    ]

drawmm :: Code -> IO ()
drawmm code = display window background (render code)
