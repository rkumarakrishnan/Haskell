module Chapter3 where

    -- Esta biblioteca só existe para a implementação Hugs do Haskell, e só funciona com a versão 98 dele (http://cvs.haskell.org/Hugs/pages/downloading-Nov2002.htm)
    import SOEGraphics

    -- Escreve o texto "Hello graphics world" numa janela
    main0 = runGraphics (
                do window <- openWindow "My first graphics program" (300, 300)
                   drawInWindow window (text (100,200) "Hello graphics world")
                   key <- getKey window
                   spaceClose window
            )
            
    -- Define uma elipse vermelha
    pic1 = withColor Red
            (ellipse (150,150) (300,200))
    
    -- Define um retângulo azul
    pic2 = withColor Blue
            (polyline [(100,50), (200,50), (200,250), (100, 250), (100,50)])
    
    -- Desenha a elipse e o retângulo imediatamente acima numa janela
    main1 = runGraphics (
                do window <- openWindow "Some graphics figures" (300, 300)
                   drawInWindow window pic1
                   drawInWindow window pic2
                   spaceClose window
            )
            
    -- Desenha um triângulo-retângulo
    fillTri :: Window -> Int -> Int -> Int -> IO ()
    fillTri w x y size = drawInWindow w (
                           withColor Blue (
                             polygon [(x,y), (x+size,y), (x,y-size), (x,y)]
                           )
                         )
                         
    -- Define o fractal triangular de Sierpinski
    minSize :: Int
    minSize = 10
    sierpinskiTri :: Window -> Int -> Int -> Int -> IO ()
    sierpinskiTri w x y size = if size <= minSize then fillTri w x y size
                                                  else let size2 = div size 2
                                                       in do sierpinskiTri w x y size2
                                                             sierpinskiTri w x (y-size2) size2
                                                             sierpinskiTri w (x+size2) y size2
            
    -- Desenha o fractal de Sierpinski definido imediatamente acima
    main2 = runGraphics (
                do window <- openWindow "Sierpinski's triangle" (400,400)
                   sierpinskiTri window 50 300 512 --256
                   spaceClose window
            )            
      
    -- Fecha a janela quando o usuário pressiona "espaço"      
    spaceClose :: Window -> IO ()
    spaceClose window = do key <- getKey window
                           if key == ' ' then closeWindow window
                                         else spaceClose window
    
    -- -------------                                    
    -- Exercício 3.2
    -- -------------
    
    upTriangle :: Window -> Int -> Int -> Int -> IO ()
    upTriangle w x y s = drawInWindow w (
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
                           
                               
    downTriangle :: Window -> Int -> Int -> Int -> IO ()
    downTriangle w x y s = drawInWindow w (
                                 withColor Blue (
                                   let a = intToFloat s / 2
                                       b = a / (sqrt 3)
                                       float_x = intToFloat x
                                       float_y = intToFloat y
                                   in polygon [(round (float_x - a), round (float_y + b)),
                                               (round (float_x), round (float_y - 2 * b)),
                                               (round (float_x + a), round (float_y + b))]
                                 )
                            )
    
    starOfDavid :: Window -> Int -> Int -> Int -> IO ()
    starOfDavid w x y s = do upTriangle w x y s
                             downTriangle w x y s                       
                           
    intToFloat :: Int -> Float
    intToFloat n = fromInteger (toInteger n)                           
                           
    snowflake :: Window -> Int -> Int -> Int -> IO ()
    snowflake w x y size = if size <= minSize then starOfDavid w x y size
                                              else let size2 = div size 3
                                                       a = intToFloat size2 / 2
                                                       b = a / (sqrt 3)
                                                       float_x = intToFloat x
                                                       float_y = intToFloat y
                                                   in do snowflake w (round (float_x - a)) (round (float_y - b)) size2
                                                         snowflake w (round (float_x)) (round (float_y + 2 * b)) size2  
                                                         snowflake w (round (float_x + a)) (round (float_y - b)) size2                         
                           
    main3 = runGraphics (
                do window <- openWindow "Snowflake fractal" (800, 800)
                   --starOfDavid window 200 200 200
                   snowflake window 400 400 800
                   spaceClose window
            )
