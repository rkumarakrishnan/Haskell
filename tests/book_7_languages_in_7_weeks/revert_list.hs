-- Exercício 2 (p. 271)
module Main where

  import Data.List

  revert_list :: [a] -> [a]
  revert_list [] = []
  revert_list (x:xs) = revert_list xs ++ [x]
  
  colors = ["red", "green", "blue"]
  
  --combine :: [a] -> [(a,a)]
  --combine list = [(x, y) | x <- list, y <- list, x /= y]
  --combine list = foldl (\(x,y) result -> if elemIndex (y,x) result == Nothing then result ++ (x,y)) [] [(x, y) | x <- list, y <- list, x /= y]
  
  my_nub_filter x list = if elemIndex x list == Nothing
                           then list ++ [x]
                           else list
                           
  combine list = foldl my_nub_filter [] [(x, y) | x <- list, y <- list, x /= y]                            