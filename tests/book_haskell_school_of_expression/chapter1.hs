listSum :: [Float] -> Float
listSum [] = 0
listSum (x:xs) = x + listSum xs

circleArea :: Float -> Float
circleArea r = pi * r^2 