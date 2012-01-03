module Chapter13 where

    import Shape
    import Region
    import Picture
    import Animation

    -- Exercício 13.4 (p. 183)
    -- -----------------------

    eR :: Behavior Float
    eR = 2.5

    mR :: Behavior Float
    mR = 0.5

    white :: Behavior Color
    white = lift0 White

    blue :: Behavior Color
    blue = lift0 Blue

    planet :: Behavior Region
    planet = shape (ell 1 1)

    sun :: Behavior Picture
    sun = reg yellow (shape (ell 0.3 0.3))
    
    earth :: Behavior Picture
    earth = reg blue (translate (eR * cos time, eR * sin time) (shape (ell 0.15 0.15)))

    moon :: Behavior Picture
    moon = reg white (
	     translate (
	       eR * cos time + mR * (timeTrans (3*time) (mR * cos time)),
	       eR * sin time + mR * (timeTrans (3*time) (mR * sin time))
	     ) (shape (ell 0.1 0.1))
	   )

    translation :: Behavior Picture
    translation = sun `over` earth `over` moon

    main8 :: IO()
    main8 = animateB "Solar system" translation
