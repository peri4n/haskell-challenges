module Problems.Problem2 where

myButLast :: [a] -> a
myButLast [] = error "An empty list has no last element."
myButLast [x] = error "An empty list has no last element."
myButLast [x, _] = x
myButLast (y:rest@(x:xs)) = myButLast rest
