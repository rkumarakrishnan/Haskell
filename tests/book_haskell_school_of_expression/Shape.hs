module Shape (Shape(Rectangle, Ellipse, RtTriangle, Polygon),
              Radius, Side, Vertex,
              square, circle, distance, area) where
              
    data Shape = Rectangle Side Side
               | Ellipse Radius Radius
               | RtTriangle Side Side
               | Polygon [Vertex]
        deriving Show

    type Side   = Float
    type Radius = Float
    type Vertex = (Float, Float)
    
    square s = Rectangle s s
    circle r = Ellipse r r
    rectangle s1 s2 = Polygon [(0,0), (s1, 0), (s1, s2), (0, s2)]
    rtTriangle s1 s2 = Polygon [(0,0), (s1, 0), (s1, s2)]
    
    area :: Shape -> Float
    area (Rectangle a b) = a * b
    area (RtTriangle a b) = a * b / 2
    area (Ellipse r1 r2) = pi * r1 * r2
    area (Polygon (v1:vs)) = polyArea vs
        where polyArea :: [Vertex] -> Float
              polyArea (v2:v3:vs') = triArea v1 v2 v3 + polyArea (v3:vs')
              polyArea _           = 0
    
    triArea :: Vertex -> Vertex -> Vertex -> Float
    triArea v1 v2 v3 = let a = distance v1 v2
                           b = distance v2 v3
                           c = distance v3 v1
                           s = (a + b + c) / 2
                       in sqrt (s * (s - a) * (s - b) * (s - c))
    
    distance :: Vertex -> Vertex -> Float
    distance (x1, y1) (x2, y2) = sqrt((x2 - x1)^2 + (y2 - y1)^2)