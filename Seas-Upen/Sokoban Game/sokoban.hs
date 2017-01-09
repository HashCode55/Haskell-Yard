{-# LANGUAGE OverloadedStrings #-}

import CodeWorld

main :: IO()
main = sokoban

{- ### SOKOBAN ### -}

-- ## THE BUILDING BLOCKS ## -- 
wall, ground, storage, box :: Picture 

wall = colored (grey 0.4) (solidRectangle 1 1) 
ground = colored yellow (solidRectangle 1 1) 
storage = colored black (solidCircle 0.3) & ground 
box = colored brown (solidRectangle 1 1) 
player = colored green (solidCircle 0.5) & ground -- the main player

-- ## Drawing the tiles ## --
data Tile = Wall  -- This is an enum!
          | Ground 
          | Storage 
          | Box 
          | Blank  
          | Player 
            deriving(Eq, Ord, Show)

drawTile :: Tile -> Picture 
drawTile x 
  | x == Wall       = wall 
  | x == Ground     = ground
  | x == Storage    = storage
  | x == Box        = box 
  | x == Player     = player
  | otherwise       = blank 

-- ## Maze function ## --
maze :: Integer -> Integer -> Tile
maze x y
  | abs x == 0 && y == 1     = Player 
  | abs x > 4  || abs y > 4  = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x ==  2 && y <= 0        = Wall
  | x ==  3 && y <= 0        = Storage 
  | x >= -2 && y == 0        = Box
  | otherwise                = Ground
  
  
-- ## Draw the maze ## --

wrapper :: (Integer -> Picture) -> Picture 
wrapper func = go (-10) -- func is a captured variable, no need to write it again 
  where 
    go 11 = blank 
    go x = func x & go (x + 1)    
    
-- for drawing the tile 
drawTileAt :: Integer -> Integer -> Picture
drawTileAt x y = translated (fromIntegral x) (fromIntegral y) (drawTile (maze x y))

pictureOfMaze :: Picture 
pictureOfMaze = wrapper drawRow 
-- more compact using lambda functions 
-- pictureOfMaze = wrapper (\r -> wrapper (drawTileAt r))
drawRow x = wrapper (drawTileAt x) -- Currying

-- ## Event Handling ## --

data Direction = R -- Right
               | L -- Left
               | U -- Up
               | D -- Down 
               deriving(Eq, Ord, Show)
               
data Coord = Coord Integer Integer -- Coordinates. This is a product type datatype

tileCoord :: Coord -> Picture -> Picture 
tileCoord (Coord x y) pic = translated (fromIntegral x) (fromIntegral y) pic -- Integer makes the code bug free 

-- Lets move the player 
movePlayer :: Coord -> Direction -> Coord
movePlayer (Coord x y) dir 
  | dir == R = Coord (x + 1) y
  | dir == L = Coord (x - 1) y
  | dir == U = Coord x (y + 1)
  | dir == D = Coord x (y - 1)
  
player_wor = 

handleTime :: Double -> Coord -> Coord -- Coord is the 'world' here
handleTime _ c = c -- we don't want to use this function, it modifies the state after certain amount of time is passed 

handleEvent :: Event -> Coord -> Coord 
handleEvent (KeyPress key) c -- handles the input event 
  | key == "Right" = movePlayer c R 
  | key == "Left" = movePlayer c L
  | key == "Up" = movePlayer c U 
  | key == "Down" = movePlayer c D 
  | otherwise = c 
   
drawState :: Coord -> Picture 
drawState c = tileCoord c pictureOfMaze -- redraws the picture 

sokoban ::IO()
sokoban = interactionOf initialCoord handleTime handleEvent drawState 

-- experi = translated 0 0 (solidCircle 1) & path [(0, 0), (0, (-3))]

-- experiment_shapes :: IO()
-- experiment_shapes = drawingOf experi