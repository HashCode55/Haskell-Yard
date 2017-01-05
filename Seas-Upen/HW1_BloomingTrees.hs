-- HW1
import CodeWorld 

main :: IO()
main = exercise2

-- Exercise-1 

topCircle c = colored c (translated 0 3 (solidCircle 1))
middleCircle c = colored c (translated 0 0 (solidCircle 1))
bottomCircle c = colored c (translated 0 (-3) (solidCircle 1))
frame = rectangle 2.5 8.5

trafficLight ::  Color -> Color -> Color -> Picture
trafficLight c1 c2 c3 = (topCircle c1) & (middleCircle c2) & (bottomCircle c3) & frame

{- ### lets mimic the real traffic light ### -}

trafficController :: Integer -> Picture 
trafficController t 
  | t == 7 = trafficLight red yellow black -- short red and amber phase 
  | t == 3 = trafficLight black yellow black -- short amber phase 
  | t < 3 = trafficLight black black green  
  | t >= 4 && t < 7 = trafficLight red black black 
   
trafficLightCount :: Double -> Picture   
trafficLightCount t = trafficController (round t `mod` 8)

exercise1 :: IO()
exercise1 = animationOf trafficLightCount 


-- Exercise-2 

-- spawn a leaf 
leaf :: Color -> Double -> Picture 
leaf c r = colored c (solidCircle r) -- r is the radius of the circle 

tree :: Integer -> Double -> Picture 
tree 0 f 
  | f <= 10 = colored yellow (leaf green (f * 0.02))
  | otherwise = colored yellow (leaf green (0.2))
tree n f = path [(0, 0), (0, 1)] & translated 0 1 (
  rotated (pi / 10) (tree (n - 1) f) & rotated (-pi/10) (tree (n - 1) f))

toDisplay = tree 8 
exercise2 ::IO()
exercise2 = animationOf toDisplay
