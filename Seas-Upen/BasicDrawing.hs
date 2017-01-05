{- 
Functional languages excel at wholemeal programming, 
a term coined by Geraint Jones. 
Wholemeal programming means to think big: 
work with an entire list, rather than a sequence of elements; 
develop a solution space, rather than an individual solution; 
imagine a graph, rather than a single path.
-}

import CodeWorld 

-- lets define at different locations 
topCircle c = colored c (translated 0 (-1.5) (solidCircle 1))
bottomCircle c = colored c (translated 0 1.5 (solidCircle 1))
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True = (topCircle green) & (bottomCircle red) & frame
trafficLight False = (topCircle red) & (bottomCircle green) & frame

{- ###lets do some animation! ### -}

trafficLightCont :: Double -> Picture 
trafficLightCont t 
  | round (t / 3) `mod` 2 == 0 = trafficLight True 
  | otherwise                  = trafficLight False 
 
-- main = animationOf trafficLightCount   


{- ### gist of recursion ### -}

lights :: Picture -> Double -> Integer -> Picture 
lights _ _ 0 = blank 
lights pic trans n = pic & translated trans 0 (lights pic trans (n-1))

-- toDisplay = lights (trafficLight True) 3 3 
-- main = drawingOf toDisplay


{- ### lets try to draw a tree now ### -}

-- path draws a line from x1, y1 to x2, y2
tree 0 _ = blank 
tree n f = path [(0, 0), (0, 1)] & translated 0 1 (
  rotated (f * pi / 10) (tree (n - 1) f) & rotated (- f * pi/10) (tree (n - 1) f))

toDisplay = tree 8 . sin 

main :: IO()
main = animationOf toDisplayimport CodeWorld

ourPicture :: Picture
ourPicture = solidCircle 1

main :: IO()
main = drawingOf ourPicture
