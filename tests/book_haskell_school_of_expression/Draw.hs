module Draw (
        inchToPixel, pixelToInch, intToFloat,
        xWin, yWin, trans, shapeToGraphic, spaceClose
    ) where

    import Shape
    import SOE
    
    inchToPixel :: Float -> Int
    inchToPixel x = round (100 * x)
    
    pixelToInch :: Int -> Float
    pixelToInch px = intToFloat px / 100
    
    intToFloat :: Int -> Float
    intToFloat n = fromInteger (toInteger n)
    
    xWin, yWin :: Int
    xWin = 600
    yWin = 500
    
    xWin2, yWin2 :: Int
    xWin2 = xWin `div` 2
    yWin2 = yWin `div` 2
    
    trans :: Vertex -> Point
    trans (x,y) = (xWin2 + inchToPixel x, yWin2 - inchToPixel y)
    
    transList :: [Vertex] -> [Point]
    transList [] = []
    transList (p:ps) = trans p : transList ps
    
    shapeToGraphic :: Shape -> Graphic
    shapeToGraphic (Rectangle s1 s2) = let s12 = s1 / 2
                                           s22 = s2 / 2
                                       in polygon (transList [(-s12, -s22), (-s12, s22), (s12, s22), (s12, -s22)])
    shapeToGraphic (Ellipse r1 r2) = ellipse (trans (-r1, -r2)) (trans (r1, r2))
    shapeToGraphic (RtTriangle s1 s2) = polygon (transList [(0,0), (s1, 0), (0, s2)])
    shapeToGraphic (Polygon vts) = polygon (transList vts)
    
    -- Fecha a janela quando o usuário pressiona "espaço"      
    spaceClose :: Window -> IO ()
    spaceClose window = do key <- getKey window
                           if key == ' ' then closeWindow window
                                         else spaceClose window
