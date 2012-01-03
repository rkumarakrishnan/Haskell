module Chapter8 where

    import Region

    -- ---------------------
    -- Exerc�cio 8.3 (p. 98)
    -- A regi�o "annulus" � um anel de raio interno r1 e raio externo r2. Ela pode
    -- ser definida com a interse��o de um c�rculo de raio r2 com o complemento do c�rculo de raio r1
    -- ---------------------
    annulus :: Radius -> Radius -> Region
    annulus r1 r2 = (Shape (Ellipse r2 r2)) `Intersect` (Complement (Shape (Ellipse r1 r1)))

    -- ---------------------
    -- Exerc�cio 8.4 (p. 98)
    -- ---------------------
    {-
       Para resolver este exerc�cio � preciso modificar a linha "and leftOfList" para "and leftOfList || and rightOfList".
       A lista rightOfList � definida de forma an�loga a leftOfList: veja no arquivo Region.hs.
     -} 

    -- ----------------------------------
    -- Exerc�cio 8.5 (p.99): no Region.hs
    -- ----------------------------------

    -- --------------------
    -- Exerc�cio 8.6 (p.99)
    -- --------------------

    createPolygonRegion :: [Coordinate] -> Region
{-    createPolygonRegion pts = if length pts < 3 then
                                Empty
                              else
                                let rays = zip pts (tail pts ++ [head pts])
                                    (plane:planes) = [HalfPlane p1 p2 | (p1,p2) <- rays]
                                in foldl Intersect plane planes-}
    createPolygonRegion pts 
                      | length pts < 3 = Empty
                      | otherwise      = let rays = zip pts (tail pts ++ [head pts])
                                             (plane:planes) = [HalfPlane p1 p2 | (p1,p2) <- rays]
                                         in foldl Intersect plane planes
  {-
     N�o deu certo. Simulando...
     createPolygonRegion [(0,0), (1,0), (1,1), (0,1)]
       = let rays = [((0,0),(1,0)),
                     ((1,0),(1,1)),
                     ((1,1),(0,1)),
                     ((0,1),(0,0))]
             planes = [HalfPlane (0,0) (1,0),
                       HalfPlane (1,0) (1,1),
                       HalfPlane (1,1) (0,1),
                       HalfPlane (0,1) (0,0)]
         in foldl Intersect Empty planes
              = Empty `Intersect` HalfPlane (0,0) (1,0)
                      `Intersect` HalfPlane (1,0) (1,1)
                      `Intersect` HalfPlane (1,1) (0,1)
                      `Intersect` HalfPlane (0,1) (0,0)

    (createPolygonRegion [(0,0), (1,0), (1,1), (0,1)) `containsR` p
      = ((((Empty `Intersect` HalfPlane (0,0) (1,0))
                  `Intersect` HalfPlane (1,0) (1,1))
                  `Intersect` HalfPlane (1,1) (0,1))
                  `Intersect` HalfPlane (0,1) (0,0)) `containsR` p
      = (((Empty `Intersect` HalfPlane (0,0) (1,0))
                 `Intersect` HalfPlane (1,0) (1,1))
                 `Intersect` HalfPlane (1,1) (0,1)) `containsR` p
        && HalfPlane (0,1) (0,0) `containsR` p
      = ((Empty `Intersect` HalfPlane (0,0) (1,0))
                `Intersect` HalfPlane (1,0) (1,1)) `containsR` p
        && HalfPlane (1,1) (0,1) `containsR` p
        && HalfPlane (0,1) (0,0) `containsR` p
      = (Empty `Intersect` HalfPlane (0,0) (1,0)) `containsR` p
        && HalfPlane (1,0) (1,1) `containsR` p
        && HalfPlane (1,1) (0,1) `containsR` p
        && HalfPlane (0,1) (0,0) `containsR` p
      = Empty `containsR` p <------------------------------ Aqui est� o erro, pois � sempre False!
        && HalfPlane (0,0) (1,0) `containsR` p
        && HalfPlane (1,0) (1,1) `containsR` p
        && HalfPlane (1,1) (0,1) `containsR` p
        && HalfPlane (0,1) (0,0) `containsR` p

        Resumindo o erro: na minha primeira tentativa, o init do foldl era Empty, de modo que ao expandir os `containsR` e `Intersect`
        sempre havia uma instru��o que retornava False sempre (acima). Para resolver, bastou tirar o Empty e usar o primeiro HalfPlane
        do vetor de HalfPlane's da solu��o: (plane:planes). Alternativamente, eu deveria ter come�ado o foldl com o complemento de
        Empty (abaixo).
    -} 

    alternativeCreatePolygonRegion :: [Coordinate] -> Region
    alternativeCreatePolygonRegion pts = if length pts < 3 then
                                            Empty
                                         else
                                            let rays = zip pts (tail pts ++ [head pts])
                                                planes = [HalfPlane p1 p2 | (p1,p2) <- rays]
                                            in foldl Intersect (Complement Empty) planes

    -- --------------------
    -- Exerc�cio 8.7 (p.99)
    -- --------------------
    
    flipX :: Region -> Region
    flipX r = Scale (-1,1) r
    -- (flipX r) `containsR` (x,y) = r `containsR` (-x,y) <-- Isto n�o constr�i uma regi�o

    flipY :: Region -> Region
    flipY r = Scale (1,-1) r

    -- ---------------------
    -- Exerc�cio 8.8 (p.101)
    -- ---------------------
    {-
        Axioma 3
        (r1 `Intersect` (r2 `Union` r3)) `containsR` p
          = (r1 `containsR` p) && ((r2 `Union` r3) `containsR` p)
          = (r1 `containsR` p) && (r2 `containsR` p || r3 `containsR` p)
          = (r1 `containsR` p && r2 `containsR` p) || (r1 `containsR` p && r3 `containsR` p)
          = (r1 `Intersect r2) `containsR` p || (r1 `Intersect` r3) `containsR` p
          = ((r1 `Intersect r2) `Union` (r1 `Intersect` r3)) `containsR` p

        Logo,
        r1 `Intersect` (r2 `Union` r3) = (r1 `Intersect r2) `Union` (r1 `Intersect` r3)

        Axioma 4a
        (r `Union` Empty) `containsR` p
          = r `containsR` p || Empty `containsR` p
          = r `containsR` p || False
          = r `containsR` p
        Logo,
        r `Union` Empty = r

        Axioma 4b
        (r `Intersect` univ) `containsR` p
          = r `containsR` p && univ `containsR` p
          = r `containsR` p && (Complement Empty) `containsR` p
          = r `containsR` p && not (Empty `containsR` p)
          = r `containsR` p && not (False)
          = r `containsR` p && True
          = r `containsR` p
        Logo,
        r `Intersect` univ = r

        Axioma 5a
        (r `Union` Complement r) `containsR` p
          = r `containsR` p || (Complement r) `containsR` p
          = r `containsR` p || not (r `containsR` p)
          = t || not t   <-- t = r `containsR` p
          = True
          = not (False)
          = not (Empty `containsR` p)
          = (Complement Empty) `containsR` p
          = univ `containsR` p
        Logo,
        r `Union` Complement r = univ

        Axioma 5a
          = univ `containsR` p
        Logo,
        r `Union` Complement r = univ

        Axioma 5b
        (r `Intersect` Complement r) `containsR` p
          = r `containsR` p && (Complement r) `containsR` p
          = r `containsR` p && not (r `containsR` p)
          = t && not t   <-- t = r `containsR` p
          = False
          = Empty `containsR` p
        Logo,
        r `Intersect` Complement r = Empty

     -}

    -- ---------------------
    -- Exerc�cio 8.9 (p.101)
    -- ---------------------
    {-
        r `Intersect` univ = r `Intersect` (r `Union` (Complement r))
                         r = r `Intersect` (r `Union` (Complement r))   (Axioma 4)
                           = (r `Intersect` r) `Union` (r `Intersect` (Complement r))   (Axioma 3)
                           = (r `Intersect` r) `Union` Empty   (Axioma 5)
                           = (r `Intersect` r)   (Axioma 4)

        J� fiz esse tipo de demosntra��o in�meras vezes...

     -}

    -- ----------------------
    -- Exerc�cio 8.10 (p.104)
    -- ----------------------
    {-
        O primeiro "fato" � falso; n�o pode ser provado. A n�o ser que se trate da redefini��o de Rectangle, que considera o m�dulo dos lados dados.

	Ellipse (-r1) r2 `containsS` (x,y)
	  = (x/(-r1))^2 + (y/r2)^2 <= 1
	  = (x/r1)^2 + (y/r2)^2 <= 1
	  = Ellipse r1 r2 `containsS` (x,y)

     -}
    
    -- ----------------------
    -- Exerc�cio 8.11 (p.105)
    -- ----------------------
    {-
	Um pol�gono qualquer � convexo se o m�dulo produto vetorial de cada par de lados consecutivos, interpretados como vetores,
	for positivo para todos esses pares (pol�gono definido no sentido anti-hor�rio); ou negativo para todos esses pares (hor�rio).
     -}

    isConvex :: Shape -> Bool
    isConvex (Polygon p1s) = and (map (>= 0) times) || and (map (<= 0) times)
			     where p2s = tail p1s ++ [head p1s]
				   vec1s = zipWith (\(x1,y1) (x2,y2) -> (x2-x1, y2-y1)) p1s p2s
				   vec2s = tail vec1s ++ [head vec1s]
				   times = [x1*y2-x2*y1 | ((x1,y1),(x2,y2)) <- zip vec1s vec2s]
    isConvex (Rectangle _ _) = True
    isConvex (RtTriangle _ _) = True
    isConvex (Ellipse _ _) = True


    polygon1, polygon2, polygon3 :: Shape
    polygon1 = Polygon [(0,0),(2,0),(2,1),(2,2),(0,2)] -- Convexo (definido no sentido anti-hor�rio)
    polygon2 = Polygon [(0,0),(2,0),(1,1),(2,2),(0,2)] -- C�ncavo (anti-hor�rio)
    polygon3 = Polygon [(0,0),(2,0),(3,1),(2,2),(0,2)] -- Convexo (anti-hor�rio)

    polygon1a, polygon2a, polygon3a :: Shape
    polygon1a = Polygon [(0,0),(0,2),(2,2),(2,1),(2,0)] -- Convexo (hor�rio)
    polygon2a = Polygon [(0,0),(0,2),(2,2),(1,1),(2,0)] -- C�ncavo (hor�rio)
    polygon3a = Polygon [(0,0),(0,2),(2,2),(3,1),(2,0)] -- Convexo (hor�rio)

    -- --------------
    -- Exerc�cio 8.11
    -- --------------

    newArea :: Shape -> Float
    newArea s
	| isConvex s = area s
	| otherwise  = error "Shape must be convex, but it is concave."

    -- --------------
    -- Exerc�cio 8.12
    -- --------------

    {-
	Se eu entendi o exerc�cio, j� o fiz, na defini��o de `containsS` para o construtor Polygon (arquivo Region.hs).
     -}

    -- --------------
    -- Exerc�cio 8.13
    -- --------------
    
