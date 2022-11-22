module Problems.Problem1 where

myLast :: [a] -> a
myLast [] = error "An empty list has no last element."
myLast [x] = x
myLast (x:xs) = myLast xs
