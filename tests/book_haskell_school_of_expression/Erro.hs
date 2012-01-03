module Erro where

    import SOEGraphics
    
    starOfDavid :: Window -> Int -> Int -> Int -> IO ()
    starOfDavid w x y s = drawInWindow w (
                                 withColor Blue (
                                   let a = intToFloat s / 2
                                       b = a / (sqrt 3)
                                       float_x = intToFloat x
                                       float_y = intToFloat y
                                   in polygon [(round (float_x - a), round (float_y - b)),
                                               (round (float_x), round (float_y + 2 * b)),
                                               (round (float_x + a), round (float_y - b))]
                                 )
                            )
    
    intToFloat :: Int -> Float
    intToFloat n = fromInteger (toInteger n)
    
    main3 = runGraphics (
            do window <- openWindow "Snowflake fractal" (400, 400)
               starOfDavid window 200 200 200
               spaceClose window
        ) 
    
    spaceClose :: Window -> IO ()
    spaceClose window = do key <- getKey window
                           if key == ' ' then closeWindow window
                                         else spaceClose window