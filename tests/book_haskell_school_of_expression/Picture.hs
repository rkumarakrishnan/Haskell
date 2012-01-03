module Picture (Picture(Region, Over, EmptyPic),
                Color(Black, Blue, Green, Cyan, Red, Magenta, Yellow, White),
                regionToGRegion, shapeToGRegion,
                drawRegionInWindow, drawPic, spaceClose,
                module Region) where

    import Draw
    import Region
    import SOE hiding (Region)
    import qualified SOE as G (Region)

    data Picture = Region Color Region
                 | Picture `Over` Picture
                 | EmptyPic
         deriving Show

    drawRegionInWindow :: Window -> Color -> Region -> IO()
    drawRegionInWindow window color region = drawInWindow window
                                                (withColor color
                                                    (drawRegion (regionToGRegion region)))

    drawPic :: Window -> Picture -> IO()
    drawPic window (Region color region) = drawRegionInWindow window color region
    drawPic window (p1 `Over` p2)        = do drawPic window p2; drawPic window p1
    drawPic window EmptyPic              = return ()

    shapeToGRegion :: Vector -> Vector -> Shape -> G.Region
    shapeToGRegion (lx,ly) (sx,sy) shape
        = case shape of
            Rectangle s1 s2  -> createRectangle (trans (-s1/2,-s2/2)) (trans (s1/2,s2/2))
            Ellipse r1 r2    -> createEllipse (trans (-r1,-r2)) (trans (r1,r2))
            Polygon vs       -> createPolygon (map trans vs)
            RtTriangle s1 s2 -> createPolygon (map trans [(0,0), (s1,0), (0,s2)])
          where trans :: Vertex -> Point
                trans (x,y) = (xWin2 + inchToPixel ((x+lx)*sx),
                               yWin2 - inchToPixel ((y+ly)*sy))

    type Vector = (Float, Float)

    regToGReg :: Vector -> Vector -> Region -> G.Region
    regToGReg loc sca (Shape s)                     = shapeToGRegion loc sca s
    regToGReg loc (sx,sy) (Scale (u,v) r)           = regToGReg loc (sx*u, sy*v) r
    regToGReg (lx,ly) (sx,sy) (Translate (dx,dy) r) = regToGReg (lx+dx*sx,ly+dy*sy) (sx,sy) r
    regToGReg loc sca Empty                         = createRectangle (0,0) (0,0)
    regToGReg loc sca (r1 `Union` r2)               = primGReg loc sca r1 r2 orRegion
    regToGReg loc sca (r1 `Intersect` r2)           = primGReg loc sca r1 r2 andRegion
    regToGReg loc sca (Complement r)                = primGReg loc sca winRect r diffRegion

    regionToGRegion :: Region -> G.Region
    regionToGRegion r = regToGReg (0,0) (1,1) r

    -- Auxiliar
    primGReg :: Vector -> Vector -> Region -> Region -> (G.Region -> G.Region -> G.Region) -> G.Region
    primGReg loc sca r1 r2 op = let gr1 = regToGReg loc sca r1
                                    gr2 = regToGReg loc sca r2
                                in op gr1 gr2

    -- Auxiliar
    winRect :: Region
    winRect = Shape (Rectangle (pixelToInch xWin) (pixelToInch yWin))

    -- Auxiliar
    xWin2, yWin2 :: Int
    xWin2 = xWin `div` 2
    yWin2 = yWin `div` 2


    -- Testes...
    
    draw :: String -> Picture -> IO()
    draw s p = runGraphics $
               do w <- openWindow s (xWin, yWin)
                  drawPic w p
                  spaceClose w
                
    
    xUnion :: Region -> Region -> Region
    r1 `xUnion` r2 = (r1 `Intersect` Complement r2) `Union`
                     (r2 `Intersect` Complement r1)

    r1, r2, r3, r4 :: Region
    r1 = Shape (Rectangle 3 2)
    r2 = Shape (Ellipse 1 1.5)
    r3 = Shape (RtTriangle 3 2)
    r4 = Shape (Polygon [(-2.5,2.5), (-3,0), (-1.7,-1), (-1.1,0.2), (-1.5,2)])

    reg1, reg2 :: Region
    reg1 = r3 `xUnion` (r1 `Intersect` Complement r2 `Union` r4)
    reg2 = let circle = Shape (Ellipse 0.5 0.5)
               square = Shape (Rectangle 1 1)
           in (Scale (2,2) circle) 
              `Union` (Translate (1,0) square)
              `Union` (Translate (-1,0) square)

    pic1, pic2, pic3 :: Picture
    pic1 = Region Blue reg1
    pic2 = Region Yellow (Translate (0,-1) reg2)
    pic3 = pic2 `Over` pic1
   
    -- A parte de user interaction
    picToList :: Picture -> [(Color,Region)]
    picToList EmptyPic       = []
    picToList (Region c r)   = [(c,r)] 
    picToList (p1 `Over` p2) = picToList p1 ++ picToList p2

    adjust :: [(Color,Region)] -> Coordinate -> (Maybe (Color,Region), [(Color,Region)])
    adjust regs p
        = case (break (\(_,r) -> r `containsR` p) regs) of
            (top, hit:rest) -> (Just hit, top ++ rest)
            (_,[])          -> (Nothing, regs)

    loop :: Window -> [(Color,Region)] -> IO()
    loop w regs =
        do clearWindow w
           sequence_ [drawRegionInWindow w c r | (c,r) <- reverse regs]
           (x,y) <- getLBP w
           case (adjust regs (pixelToInch(x-xWin2), pixelToInch(yWin2-y))) of
               (Nothing, _)        -> closeWindow w
               (Just hit, newRegs) -> loop w (hit:newRegs)

    draw2 :: String -> Picture -> IO()
    draw2 s p = runGraphics $
                do w <- openWindow s (xWin, yWin)
                   loop w (picToList p)

    p1, p2, p3, p4 :: Picture
    p1 = Region Red r1
    p2 = Region Blue r2
    p3 = Region Green r3
    p4 = Region Yellow r4

    pic :: Picture
    pic = foldl Over EmptyPic [p1,p2,p3,p4]
    main = draw2 "Picture click test" pic
