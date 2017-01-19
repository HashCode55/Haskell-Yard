{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main :: IO()
main = sokoban

-- ################################################## --
-- ###          S   O   K   O   B   A   N         ### --
-- ################################################## --


-- ################ --
-- ## SETTING UP ## --
-- ################ --

-- ## REQUIRED TYPES ## --
data List a = Entry a (List a) -- Recursive types 
            | Empty
            
-- ## THE BUILDING BLOCKS ## -- 
wall, ground, storage, box :: Picture 
wall = colored (grey 0.4) (solidRectangle 1 1) 
ground = colored yellow (solidRectangle 1 1) 
storage = colored black (solidCircle 0.3) & ground 
box = colored brown (solidRectangle 1 1) 

-- ## PLAYER DETAILS ## --
player :: Direction -> Picture 
player d
  | d == R = colored green (solidCircle 0.5) -- the main player
  | d == L = colored red (solidCircle 0.5)
  | d == U = colored green (solidCircle 0.5)
  | d == D = colored red (solidCircle 0.5)


data Direction = R -- Right
               | L -- Left
               | U -- Up
               | D -- Down 
               deriving(Eq, Ord, Show)
               
data Coord = Coord Integer Integer -- Coordinates. This is a product type datatype

-- ## MAZE SETUP ## --
data Tile = Wall  -- This is an enum!
          | Ground 
          | Storage 
          | Box 
          | Blank   
            deriving(Eq, Ord, Show)

drawTile :: Tile -> Picture 
drawTile x 
  | x == Wall       = wall 
  | x == Ground     = ground
  | x == Storage    = storage
  | x == Box        = box 
  | otherwise       = blank 

maze :: Coord -> Tile
maze (Coord x y)
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage 
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground

-- ## GAME STATES ## --
startScreen :: Picture
startScreen = scaled 3 3 (text "Sokoban") 

-- As the interactionOf is polymorphic it doesn't know what type its showing 
-- So lets exploit it.
data CurrentState world = StartScreen  -- This is same as game states in imperitive paradigm
                        | Running world
             
  
  
-- ###################### --
-- ## DRAWING THE MAZE ## --
-- ###################### --

wrapper :: (Integer -> Picture) -> Picture 
wrapper func = go (-10) -- func is a captured variable, no need to write it again 
  where 
    go 11 = blank 
    go x = func x & go (x + 1)    
    
-- for drawing the tile 
drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze (Coord x y)))

pictureOfMaze :: Picture 
-- more compact using lambda functions 
pictureOfMaze = wrapper (\r -> wrapper (drawTileAt r))


-- #################### --
-- ## EVENT HANDLING ## --
-- #################### --

type World = (Coord, Direction) -- State of the world IS defined by coordinates of player and its direction


tileCoord :: World -> (Direction -> Picture) -> Picture 
tileCoord ((Coord x y), d) pic = translated (fromIntegral x) (fromIntegral y) (pic d) -- Integer makes the code bug free 

-- ## MOVING THE PLAYER ## --
isVisitable :: Tile -> Bool 
isVisitable Storage = True 
isVisitable Ground  = True 
isVisitable _       = False 

-- Lets move the player 
movePlayer :: World -> Direction -> World
movePlayer ((Coord x y), d) dir 
  | dir == R && isVisitable (maze (Coord (x + 1) y)) = ((Coord (x + 1) y), R)                                             
  | dir == L && isVisitable (maze (Coord (x - 1) y)) = ((Coord (x - 1) y), L)
  | dir == U && isVisitable (maze (Coord x (y + 1))) = ((Coord x (y + 1)), U)
  | dir == D && isVisitable (maze (Coord x (y - 1))) = ((Coord x (y - 1)), D)
  | otherwise = ((Coord x y), d)

-- ## HIGHER ORDER FUNCTIONS FOR `interactionOf` FUNCTION ## --
initialCoord :: World 
initialCoord = ((Coord 0 1), R)

handleEvent :: Event -> World -> World 
handleEvent (KeyPress key) w -- handles the input event 
  | key == "Right" = movePlayer w R 
  | key == "Left" = movePlayer w L
  | key == "Up" = movePlayer w U 
  | key == "Down" = movePlayer w D 
handleEvent _ w = w
   
drawState :: World -> Picture 
drawState (c, d) = (tileCoord (c, d) player) & pictureOfMaze -- redraws the picture 

data Interaction world = Interaction
  world 
  (Double -> world -> world)
  (Event -> world -> world)
  (world -> Picture)
  
-- ############### --
-- ## FINAL RUN ## --
-- ############### --

resetable :: Interaction s -> Interaction s
resetable (Interaction ini hTime hEvent d)
  = Interaction ini hTime hEvent' d
  where hEvent' (KeyPress key) _ | key == "Esc" = ini
        hEvent' e              s                = hEvent e s  

startScreenState :: Interaction s -> Interaction (CurrentState s) -- redefing the main function, polymorophic one.     
startScreenState (Interaction state0 hTime hEvent drawS)
  = Interaction state0' hTime' hEvent' drawS'
  where state0' = StartScreen -- Interaction data type is polymorphic so here s is of type "CurrentState"  
        
        hTime' _ StartScreen           = StartScreen 
        hTime' t (Running s)           = Running (hTime t s)
        
        hEvent' (KeyPress key) StartScreen | key == " "           = Running state0
        hEvent' _              StartScreen                        = StartScreen 
        hEvent' e              (Running s)                        = Running (hEvent e s)
        
        drawS' StartScreen = startScreen 
        drawS' (Running s) = drawS s 
        
runInteraction :: Interaction s -> IO()        
runInteraction (Interaction ini hTime hEvent d)  
   = interactionOf ini hTime hEvent d
   
sokoban :: IO()
sokoban = runInteraction (resetable (startScreenState (Interaction initialCoord (\_ w -> w) handleEvent drawState)))
        