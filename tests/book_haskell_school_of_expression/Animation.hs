module Animation where

    import Shape
    import Draw
    import Picture
    import SOE hiding (Region)
    import qualified SOE as G (Region)
    --import Win32Misc (timeGetTime)
    --import Word (word32ToInt)

    type Animation a = Time -> a
    type Time        = Float

    -- Animation Shape
    rubberBall :: Animation Shape
    rubberBall t = Ellipse (sin t) (cos t)

    -- Animation String
    tellTime :: Animation String
    tellTime t = "The time is " ++ show t

    -- Animation Region
    revolvingBall :: Animation Region
    revolvingBall t = let ball = Shape (Ellipse 0.2 0.2)
                      in Translate (sin t, cos t) ball

    -- Animation Picture
    planets :: Animation Picture
    planets t = let p1 = Region Red (Shape (rubberBall t))
                    p2 = Region Yellow (revolvingBall t)
                in p1 `Over` p2

    -- A função animate é que atualiza a tela (a cada 30ms) com um Graphic dependente do tempo (acima).
    animate :: String -> Animation Graphic -> IO()
    animate title animation
        = runGraphics $
          do w <- openWindowEx title (Just (0,0)) (Just (xWin,yWin)) drawBufferedGraphic -- (Just 30) -- SOEGraphics --> SOE 
             t0 <- timeGetTime
             let loop = do t <- timeGetTime
                           let ft = intToFloat (word32ToInt (t-t0)) / 1000
                           setGraphic w (animation ft)
                           --getWindowTick w  -- SOEGraphics --> SOE
			   spaceCloseEx w loop -- SOEGraphics --> SOE
             loop

    -- A animação de Shape e String pode ser feita imediatamente, pois já temos tudo o que é preciso:
    main1 :: IO()
    main1 = animate "Animated shape" (withColor Blue . shapeToGraphic . rubberBall)

    main2 :: IO()
    main2 = animate "Animated text" (text (100,200) . tellTime)

    -- Mas para fazer a animação de uma região, precisamos criar uma função que transforme Region em Graphic:
    regionToGraphic :: Region -> Graphic
    regionToGraphic = drawRegion . regionToGRegion  -- Note que foi usado currying no único argumento de regionToGraphic

    main3 :: IO()
    main3 = animate "Animated region" (withColor Yellow . regionToGraphic . revolvingBall)

    -- Desenvolvimento análogo deve ser feito no caso de Picture... só dá um pouco mais de trabalho:
    pictureToGraphic :: Picture -> Graphic
    pictureToGraphic (Region c r) = withColor c (regionToGraphic r)
    pictureToGraphic (p1 `Over` p2) = pictureToGraphic p1 `overGraphic` pictureToGraphic p2
    pictureToGraphic EmptyPic = emptyGraphic

    main4 :: IO()
    main4 = animate "Animated picture" (pictureToGraphic . planets)

    -- Definindo uma categoria mais geral que animação: behavior (por que é mais geral?)
    newtype Behavior a = Beh (Time -> a)

    -- Animador para Behavior Picture
    animateB :: String -> Behavior Picture -> IO()
    animateB s (Beh pf) = animate s (pictureToGraphic . pf)

    -- Todas as definições abaixo são necessárias para fazer com que (Behavior a) seja uma instância das classes Eq, Show, Num, Fractional e Floating, que por sua vez é necessário para que possamos fazer contas com Behaviors. De outro modo teríamos de extrair de um Behavior qualquer o tipo qualificado dele, fazer as contas necessárias e, em seguida, recriar o Behavior com um novo valor.
    instance Eq (Behavior a) where
        a1 == a2 = error "Can't compare behaviors."

    instance Show (Behavior a) where
        showsPrec n a1 = error "<< Behavior >>"

    instance Num a => Num (Behavior a) where
        (+)         = lift2 (+)
        (*)         = lift2 (*)
        negate      = lift1 negate
        abs         = lift1 abs
        signum      = lift1 signum
        fromInteger = lift0 . fromInteger

    instance Fractional a => Fractional (Behavior a) where
        (/) = lift2 (/)
        fromRational = lift0 . fromRational

    instance Floating a => Floating (Behavior a) where
        pi    = lift0 pi
        sqrt  = lift1 sqrt
        exp   = lift1 exp
        log   = lift1 log
        sin   = lift1 sin
        cos   = lift1 cos
        tan   = lift1 tan
        asin  = lift1 asin
        acos  = lift1 acos
        atan  = lift1 atan
        sinh  = lift1 sinh
        cosh  = lift1 cosh
        tanh  = lift1 tanh
        asinh = lift1 asinh
        acosh = lift1 acosh
        atanh = lift1 atanh


    -- Estas funções, usadas logo acima, são usadas para TRANSFORMAR UM TIPO (A) NUM (BEHAVIOR A)
    lift0 :: a -> Behavior a
    lift0 x = Beh (\t -> x)

    lift1 :: (a -> b) -> Behavior a -> Behavior b
    lift1 f (Beh a) = Beh (\t -> f (a t))

    lift2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
    lift2 f (Beh a) (Beh b) = Beh (\t -> f (a t) (b t))

    lift3 :: (a -> b -> c -> d) -> Behavior a -> Behavior b -> Behavior c -> Behavior d
    lift3 f (Beh a) (Beh b) (Beh c) = Beh (\t -> f (a t) (b t) (c t))

    time :: Behavior Time
    time = Beh (\t -> t)

    {-
        time + 5
        Beh(\t -> t) + Beh(\t -> 5)
        (+) Beh(\t -> t) Beh(\t -> 5)
        lift2 (+) Beh(\t -> t) Beh(\t -> 5)
        Beh(\t -> (+) ((\t -> t) t) ((\t -> 5) t)
        Beh(\t -> (+) t 5)
        Beh(\t -> t + 5)
     -}

    class Monoid a where
        empty :: a
        over :: a -> a -> a

    instance Monoid [a] where
        empty = []
        over = (++)

    instance Monoid (Fun a) where
        empty = Fun id
        Fun a `over` Fun b = Fun (a . b)

    newtype Fun a = Fun (a -> a)

    instance Monoid Picture where
        empty = EmptyPic
        over = Over

    instance Monoid a => Monoid (Behavior a) where
        empty = lift0 empty
        over = lift2 over

    -- Esta função transforma vários monóides num só
    overMany :: Monoid a => [a] -> a
    overMany = foldr over empty

    -- "reg" é a versão Behavior de "Region", e assim por diante...
    reg = lift2 Region
    shape = lift1 Shape
    ell = lift2 Ellipse
    red = lift0 Red
    yellow = lift0 Yellow
    translate (Beh a1, Beh a2) (Beh r) = Beh (\t -> Translate (a1 t, a2 t) (r t))

    revolvingBallB :: Behavior Picture
    revolvingBallB = let ball = shape (ell 0.2 0.2)
		     in reg red (translate (sin time, cos time) ball)

    {-
	ball = shape (ell 0.2 0.2)
	     = shape (lift2 Ellipse 0.2 0.2)
	     = shape (lift2 Ellipse (Beh(\t -> 0.2) (Beh(\t -> 0.2))) <-- Aqui eu imagino que o Haskell ENTENDA 0.2 como Beh(\t -> 0.2)
	     = shape Beh(\t -> Ellipse ((\t -> 0.2) t) ((\t -> 0.2) t))
	     = lift1 Shape Beh(\t -> Ellipse ((\t -> 0.2) t) ((\t -> 0.2) t))
	     = Beh(\t -> Shape ((\t -> Ellipse ((\t -> 0.2) t) ((\t -> 0.2) t)) t))
	     = Beh(\t -> Ellipse 0.2 0.2)

	reg red (translate (sin time, cos time) ball) =
	Vamos por partes...

	sin time :: Behavior Time
	sin time = lift1 sin Beh(\t -> t)
		 = Beh(\t -> sin ((\t -> t) t))
		 = Beh(\t -> sin t)

	Analogamente,
	cos time :: Behavior Time
	cos time = Beh(\t -> cos t)

	translate (sin time, cos time) ball :: Behavior Region
	translate (sin time, cos time) ball =
	    = translate (Beh(\t -> sin t), Beh(\t -> cos t)) Beh(\t -> Ellipse 0.2 0.2)
	    = Beh (\t -> Translate ((\t -> sin t) t, (\t -> cos t) t) ((\t -> Ellipse 0.2 0.2) t))
	    = Beh (\t -> Translate (sin t, cos t) (Ellipse 0.2 0.2))

	reg red (translate (sin time, cos time) ball) :: Behavior Region
	reg red (translate (sin time, cos time) ball) =
	    = lift2 Region (lift0 Red) (Beh (\t -> Translate (sin t, cos t) (Ellipse 0.2 0.2)))
	    = lift2 Region (Beh(\t -> Red)) (Beh(\t -> Translate (sin t, cos t) (Ellipse 0.2 0.2)))
	    = Beh(\t -> Region ((\t -> Red) t) ((\t -> Translate (sin t, cos t) (Ellipse 0.2 0.2)) t))
	    = Beh(\t -> Region Red ((\t -> Translate (sin t, cos t) (Ellipse 0.2 0.2)) t))

	Ok! Agora eu entendi.

     -}

    main5 :: IO ()
    main5 = animateB "Revolving Ball Behavior" revolvingBallB

    -- Permite comparar dois Behavior
    (>*) :: Ord a => Behavior a -> Behavior a -> Behavior Bool
    (>*) = lift2 (>)

    -- Behavior "function"
    ifFun :: Bool -> a -> a -> a
    ifFun p c a = if p then c else a

    cond :: Behavior Bool -> Behavior a -> Behavior a -> Behavior a
    cond = lift3 ifFun

    flash :: Behavior Color
    flash = cond (sin time >* 0) red yellow

    revolvingFlashingBallB :: Behavior Picture
    revolvingFlashingBallB = let ball = shape (ell 0.2 0.2)
		             in reg flash (translate (sin time, cos time) ball)

    main5a :: IO()
    main5a = animateB "Revolving flashing Ball Behavior" revolvingFlashingBallB

    -- Behavior Time é uma função do tipo Time->Time, pois Behavior a = (\t -> a). Assim, "f" abaixo é uma função que
    -- transforma tempo em tempo, que então é usado em Behavior a (segundo argumento).
    timeTrans :: Behavior Time -> Behavior a -> Behavior a
    timeTrans (Beh f) (Beh a) = Beh (\t -> a (f t)) -- = Beh(a.f)

    flashingBall :: Behavior Picture
    flashingBall = let ball = shape (ell 0.2 0.2)
		   in reg (timeTrans (8*time) flash) (translate (sin time, cos time) ball)

    {-
	timeTrans (8*time) flash =
	    = timeTrans (8*Beh(\t->t)) flash
	    = timeTrans ((*) Beh(\t->8) Beh(\t->t)) flash
	    = timeTrans (lift2 (*) Beh(\t->8) Beh(\t->t)) flash
	    = timeTrans Beh(\t -> (*) ((\t->8) t) ((\t->t) t)) flash
	    = timeTrans Beh(\t -> (*) 8 t) flash
	    = timeTrans Beh(\t -> 8*t) flash
	    = timeTrans Beh(\t -> 8*t) Beh(\t -> if (sin t > 0) then Red else Yellow)  <--- veja abaixo a dedução de "flash"
	    = Beh(\t -> (\t -> if (sin t > 0) then Red else Yellow) . (\t -> 8*t) t)
	    = Beh(\t -> (\t -> if (sin t > 0) then Red else Yellow) (8*t))
	    = Beh(\t -> if (sin (8*t) > 0) then Red else Yellow)

	flash = cond (sin time >* 0) red yellow
	      = cond (sin Beh(\t->t) >* Beh(\t->0)) (lift0 Red) (lift0 Yellow)
	      = cond ((>*) (lift1 sin Beh(\t->t)) (Beh(\t->0))) (lift0 Red) (lift0 Yellow)
	      = lift3 ifFun ((>*) (lift1 sin Beh(\t->t)) (Beh(\t->0))) (lift0 Red) (lift0 Yellow)
	      = lift3 ifFun ((>*) Beh(\t -> sin ((\t->t) t)) Beh(\t->0)) Beh(\t->Red) Beh(\t->Yellow)
	      = lift3 ifFun (lift2 (>) Beh(\t -> sin ((\t->t) t)) Beh(\t->0)) Beh(\t->Red) Beh(\t->Yellow)
	      = lift3 ifFun Beh(\t-> (>) ((\t -> sin ((\t->t) t)) t) ((\t->0) t)) Beh(\t->Red) Beh(\t->Yellow)
	      = lift3 ifFun Beh(\t -> (>) ((\t->sin t) t) (0)) Beh(\t->Red) Beh(\t->Yellow)
	      = lift3 ifFun Beh(\t -> sin t > 0) Beh(\t->Red) Beh(\t->Yellow)
	      = Beh(\t -> ifFun ((\t -> sin t > 0) t) ((\t->Red) t) ((\t->Yellow) t))
	      = Beh(\t -> ifFun (sin t > 0) Red Yellow)
	      = Beh(\t -> if (sin t > 0) then Red else Yellow)
     -}

    main6 :: IO()
    main6 = animateB "Flashing ball" flashingBall

    revolvingBalls :: Behavior Picture
    revolvingBalls = overMany [timeTrans (lift0 (t*pi/4) + time) flashingBall | t <- [0..7]]

    main7 :: IO()
    main7 = animateB "Lots of flashing balls" revolvingBalls



    class Turnable a where
	turn :: Float -> a -> a

    instance Turnable Picture where
	turn theta (Region c r) = Region c (turn theta r)
	turn theta (p1 `Over` p2) = turn theta p1 `Over` turn theta p2
	turn theta EmptyPic = EmptyPic

    instance Turnable a => Turnable (Behavior a) where
	turn theta (Beh b) = Beh(turn theta . b)

    rotate :: Float -> Coordinate -> Coordinate
    rotate theta (x,y) = (x*c+y*s,y*c-x*s)
			 where (s,c) = (sin theta, cos theta)

    instance Turnable Shape where
	turn theta (Polygon ps) = Polygon (map (rotate theta) ps)
	
    instance Turnable Region where
	turn theta (Shape sh) = Shape (turn theta sh)


    -- Esta função eu tive de copiar do código atualizado do livro, para adaptar a SOEGraphics do Hugs para a SOE do GHC
    spaceCloseEx w loop
	= do k <- maybeGetWindowEvent w
	     case k of
		Just (Key c d) | c == ' ' && d -> closeWindow w
		Just Closed -> closeWindow w
		Nothing -> loop
		_ -> spaceCloseEx w loop
