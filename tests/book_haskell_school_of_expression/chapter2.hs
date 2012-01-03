module Chapter2 (innerProd, vectorLength, angle, convex) where

    import Shape

    -- --------------------
    -- Exercício 2.4 (p 33)
    -- --------------------
    type Vector = (Float, Float)

    -- Calcula o produto interno (inner; escalar) de dois vetores
    innerProd :: Vector -> Vector -> Float
    innerProd (v1x, v1y) (v2x, v2y) = v1x * v2x + v1y * v2y

    -- Calcula o comprimento de um vetor
    vectorLength :: Vector -> Float
    vectorLength v = sqrt (innerProd v v)

    -- Calcula o ângulo entre dois vetores
    angle :: Vector -> Vector -> Float
    angle v1 v2 = acos (innerProd v1 v2 / (vectorLength v1 * vectorLength v2))

    -- Identifica se o ângulo no vértice v2 é convexo (< 180º)
    isVertexConvex :: Vertex -> Vertex -> Vertex -> Bool
    isVertexConvex (v1x,v1y) (v2x,v2y) (v3x,v3y) = angle vec1 vec2 <= pi
                                                   where vec1 = (v1x - v2x, v1y - v2y) :: Vector
                                                         vec2 = (v3x - v2x, v3y - v2y) :: Vector

    -- Identifica se TODOS os vértices de uma lista têm ângulos convexos
    myZipWith :: (Vertex -> Vertex -> Vertex -> Bool) -> [Vertex] -> [Vertex] -> [Vertex] -> Bool
    myZipWith f [] [] [] = True
    myZipWith f (v1:v1s) (v2:v2s) (v3:v3s) = f v1 v2 v3 && myZipWith f v1s v2s v3s

    -- Identifica se um Shape é convexo
    convex :: Shape -> Bool
    convex (Rectangle s1 s2) = True
    convex (Ellipse r1 r2) = True
    convex (RtTriangle s1 s2) = True
    convex (Polygon vs) = myZipWith isVertexConvex vleft vs vright
                          where vleft  = [last vs] ++ init vs
                                vright = tail vs ++ [head vs]

    -- Teste apenas
    p1 :: Shape
    p1 = Polygon [(0,1), (0,0), (1,0), (1,1), (0.5, 0.5)]

    -- Hi hi hi... Não funciona! Esqueci: eu usei o produto interno de dois vetores para calcular o
    -- ângulo entre as arestas do polígono. Mas deste modo sempre terei ângulos menores que 180º e,
    -- portanto, nunca detectarei um polígono convexo assim. Tenho de usar outra função "angle".


    -- --------------------
    -- Exercício 2.5 (p 33)
    -- --------------------
    trapezoidArea :: Vertex -> Vertex -> Float
    trapezoidArea (x1, y1) (x2, y2) = (y2 + y1) * (x2 - x1) / 2

    -- Esta versão usa funções de ordem superior para calcular a área do polígono
    polygonArea :: Shape -> Float
    polygonArea (Polygon vs) = foldr (+) 0 (zipWith trapezoidArea vs vs_shift)
      where vs_shift = tail vs ++ [head vs]

    -- Esta versão usa recursividade para calcular a área do polígono
    recursivePolygonArea :: Shape -> Float
    recursivePolygonArea (Polygon (v1:vs)) = byVertexes (v1:vs)
        where byVertexes :: [Vertex] -> Float
              byVertexes (v2:v3:vs') = trapezoidArea v2 v3 + byVertexes (v3:vs')
              byVertexes (v4:v5:[]) = trapezoidArea v4 v5 + trapezoidArea v5 v1


    -- Analisando o funcionamento da função recursivePolygonArea
    --
    -- p2 = Polygon [(0,1),(1,0),(0,0)]
    -- recursivePolygonArea p2 = recursivePolygonArea (Polygon ((0,1):[(1,0),(0,0)]))
    --                         = byVertexes ((0,1):[(1,0),(0,0)])
    --                         = byVertexes ((0,1):(1,0):[(0,0)])
    --                         = trapezoidArea (0,1) (1,0) + byVertexes ((1,0):[(0,0)])
    --                         =                           + byVertexes ((1,0):(0,0):[])
    --                         =                           + trapezoidArea (1,0) (0,0) + trapezoidArea (0,0) (0,1)
    --                         = (0+1)*(1-0)/2 + (0+0)*(0-1)/2 + (1+0)*(0-0)/2
    --                         = 0.5
