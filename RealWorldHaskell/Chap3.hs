-- file Chap3.hs

-- some imports 
import Data.List (sortBy)


{- Q1-2:Number of elements in a list -}

myLength :: [a] -> Int -- type signature 
myLength xs = if null xs
              then 0
              else 1 + (myLength (tail xs))

myLength2 (x:xs) = 1 + myLength2 xs
myLength2 _ = 0


{- Q3:Function to compute mean of the list -}
-- TODO: type signature problem 

computeMean xs = sum xs / fromIntegral (myLength xs)


{- Q4:Convert a given list to palindrome -}

revList :: [a] -> [a]
revList xs = if null xs
             then []
             else revList (tail xs) ++ [(head xs)]
                  
convToPalin :: [a] -> [a] 
convToPalin xs = xs ++ (revList xs)


{- Q5:Check for Palindrome -}

checkPalin xs = if xs == (revList xs)
                then True
                else False


{- Q6:Sort a list based on length of each sublist -}

sortList xs = sortBy compareLen xs
              where compareLen a b = length a `compare` length b


{- Q7-8:Join a list of lists using a seprator value -}

intersperse _ y | (length y) == 1 = (head y)
intersperse x (y:xs) = y ++ [x] ++ (intersperse x xs)
intersperse _ [] = []


{- Q9:Height of a binary tree -}

data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving(Show)


heightBinTree (Node a xs ys) = maximum ((1 + heightBinTree xs), (1 + heightBinTree ys)) 
heightBinTree Empty = 0


{- Q10-13:Conver Hull Algorithm -}

data Direction = LeftDir
               | RightDir
               | Straight
               deriving(Eq, Show) -- Eq makes it comparable

-- Define a point 
data Point = Point {
  x :: Float,
  y :: Float
  } deriving(Eq, Show)
   
-- To calculate the turn we need to calculate the angle
getDirection :: Point -> Point -> Point -> Direction 
getDirection a b c 
  | z > 0 = LeftDir
  | z < 0 = RightDir
  | z == 0 = Straight
  where bax = (x b) - (x a)
        cay = (y c) - (y a)
        bay = (y b) - (y a)
        cax = (x c) - (x a)
        z = bax*cay - bay*cax

-- Compute the directions of triplets of given points 
computeDirections :: [Point] -> [Direction]
computeDirections (x:y:z:xs) = [getDirection x y z] ++ computeDirections (y:z:xs)
computeDirections xs | (length xs) < 3 = []
computeDirections [] = []
        
-- Finally implementing Graham's Scan
