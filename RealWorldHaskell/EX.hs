-- file practice.hs

{- here a is the culprit giving the
facility of parametrized type
Maybe is just a type constructor
defined in the Standard Prelude -}

data List a = Cons a (List a)
            | Nil
              deriving(Show) -- This is just to print the value in stdout 

-- Similarly lets define a tree Node

data Tree a = Node a (Tree a) (Tree a)
            | Empty
              deriving(Show)

-- Convert the value of type [a] to List a
fromList (x:xs) = Cons x (fromList xs)
fromList [] = Nil

-- Reverse of the above function
-- takes any value of List a and converts to
-- type [a]
-- ALSO - Keep in mind that pattern matching doesn't expect
-- any type constructor.
backToList (Cons x xs) = x : (backToList xs)   
backToList Nil = [] -- [] in haskell means empty/nothing

-- In the value constructor only the data constructor can be used
-- not the value constructor 
data TreeNew a = NodeNew a (Maybe (TreeNew a)) (Maybe (TreeNew a))
                 deriving(Show)

