module DrawTest where

    import Shape
    import Draw
    import SOEGraphics
    
    sh1, sh2, sh3, sh4 :: Shape
    sh1 = Rectangle 3 2
    sh2 = Ellipse 1 1.5
    sh3 = RtTriangle 3 2
    sh4 = Polygon [(-2.5,2.5), (-1.5,2.0), (-1.1,0.2), (-1.7,-1.0), (-3.0, 0)]
    
    main0 = runGraphics (
                do window <- openWindow "Drawing shapes" (xWin, yWin)
                   drawInWindow window (withColor Red (shapeToGraphic sh1))
                   drawInWindow window (withColor Blue (shapeToGraphic sh2))
                   spaceClose window
            )
            
    type ColoredShapes = [(Color, Shape)]
    
    shs :: ColoredShapes
    shs = [(Red, sh1), (Blue, sh2), (Yellow, sh3), (Magenta, sh4)]
    
    drawShapes :: Window -> ColoredShapes -> IO ()
    drawShapes window [] = return ()
    drawShapes window ((c,s):cs) = do drawInWindow window (withColor c (shapeToGraphic s))
                                      drawShapes window cs
                                      
    main1 = runGraphics (
                do window <- openWindow "Drawing shapes" (xWin, yWin)
                   drawShapes window shs
                   spaceClose window
            )     