import CodeWorld

main :: IO()
main = sokoban

{- ### SOKOBAN ### -}

-- ## THE BUILDING BLOCKS ## -- 
wall, ground, storage, box :: Picture 

wall = colored blue (solidRectangle 1 1) -- 0
ground = colored green (solidRectangle 1 1) -- 1
storage = colored black (solidCircle 0.5) -- 2
box = colored brown (solidRectangle 1 1) -- 3


-- ## Drawing the tiles ## --
drawTile :: Integer -> Picture 
drawTile x 
  | x == 1 = wall 
  | x == 2 = ground
  | x == 3 = storage
  | x == 4 = box 
  | x == 0 = blank 

-- ## Maze functio ## --
maze :: Double -> Double -> Integer
maze x y
  | abs x > 4  || abs y > 4  = 0
  | abs x == 4 || abs y == 4 = 1
  | x ==  2 && y <= 0        = 1
  | x ==  3 && y <= 0        = 3
  | x >= -2 && y == 0        = 4
  | otherwise                = 2
  
  
-- ## Draw the maze ## --
pictureOfMaze :: Picture 
pictureOfMaze = let drawCell x 10 = translated x 10 (drawTile (maze x 10))
                    drawCell x y = (translated x y (drawTile (maze x y))) & drawCell x (y + 1)
                    
                    drawRow 10 y = drawCell 10 y
                    drawRow x y = (drawCell x y) & drawRow (x + 1) y
                in  drawRow (-10) (-10)
  
  
sokoban ::IO()
sokoban = drawingOf pictureOfMaze