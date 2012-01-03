module Region (Region(Shape, Translate, Scale, Complement, Union, Intersect, Empty, HalfPlane),
               Coordinate,
               containsS, containsR,
               module Shape) where

    import Shape

    -- Uma "regi�o" pode ser:
    data Region = Shape Shape -- Uma forma (primitiva)
                | Translate Vector Region -- Uma regi�o transladada
                | Scale Vector Region -- Uma regi�o ampliada/reduzida
                | Complement Region -- O inverso de uma regi�o
                | Region `Union` Region -- A uni�o de duas regi�es
                | Region `Intersect` Region -- A interse��o de duas regi�es
                | Empty -- Uma regi�o vazia
                | HalfPlane Coordinate Coordinate -- Semi-plano
                deriving Show

    type Vector = (Float, Float)
    type Coordinate = (Float, Float)
    type Ray = (Coordinate, Coordinate)

    infixr 5 `Union`
    infixr 6 `Intersect`

    -- Defini��o da fun��o "containsS"
    -- -------------------------------
    containsS :: Shape -> Coordinate -> Bool
    (Rectangle s1 s2)  `containsS` (x,y) = let t1 = s1/2
                                               t2 = s2/2
                                           in -t1 <= x && x <= t2 && -t2 <= y && y <= t2
    (Ellipse r1 r2)    `containsS` (x,y) = (x/r1)^2 + (y/r2)^2 <= 1
    (Polygon pts)      `containsS` p     = let rays = zip pts (tail pts ++ [head pts])
                                               leftOfList = map (isLeftOf p) rays
                                               rightOfList = map (isRightOf p) rays
                                           in and leftOfList || and rightOfList
    (RtTriangle s1 s2) `containsS` p     = (Polygon [(0,0), (s1,0), (0,s2)]) `containsS` p

    -- Defini��o da fun��o "containsR"
    -- -------------------------------
    containsR :: Region -> Coordinate -> Bool
    (Shape s)             `containsR` p     = s `containsS` p
    (Translate (dx,dy) r) `containsR` (x,y) = r `containsR` (x-dx, y-dy)
    (Scale (sx,sy) r)     `containsR` (x,y) = r `containsR` (x/sx, y/sy)
    (Complement r)        `containsR` p     = not (r `containsR` p)
    (r1 `Union` r2)       `containsR` p     = r1 `containsR` p || r2 `containsR` p
    (r1 `Intersect` r2)   `containsR` p     = r1 `containsR` p && r2 `containsR` p
    Empty                 `containsR` p     = False
    (HalfPlane p1 p2)     `containsR` p     = p `isLeftOf` (p1,p2)

    -- Fun��o auxiliar
    isLeftOf, isRightOf :: Coordinate -> Ray -> Bool
    (px,py) `isLeftOf` ((ax,ay),(bx,by)) = let (s,t) = (px - ax, py - ay)
                                               (u,v) = (px - bx, py - by)
                                           in s * v >= t * u
    point `isRightOf` ray = not (point `isLeftOf` ray)


