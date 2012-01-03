module Kaleidoscope where

    import Shape
    import Region
    import Picture
    import Animation

    slowTime = 0.1 * time

    kaleido :: Integer -> (Float -> Behavior Coordinate) -> Behavior Picture
    kaleido n f = lift2 turn (pi * sin slowTime) $
		  overMany (zipWith reg (map lift0 (cycle spectrum)) (map (flip turn poly) rads))
		  where rads = map (((2*pi/fromInteger n)*).fromInteger) [0..n-1]
		        poly = polyShapeAnim (map f rads)

    kaleido1 = kaleido 6 star
	where star x = syncPair (2*cos(v*c+l), 2*abs(sin (slowTime*s-l)))
		where v = lift0 x
		      l = v * (slowTime + 1)
		      (s,c) = (sin l, cos l)

    kaleido2 = kaleido 9 star
	where star x = syncPair (2*abs(sin(v*a+slowTime)), 2*abs(cos(a+slowTime)))
		where v = lift0 x
		      a = v + slowTime*sin(v*slowTime)

    syncList :: [Behavior a] -> Behavior [a]
    syncList l = Beh(\t -> map (\(Beh f) -> f t) l)

    syncPair :: (Behavior a, Behavior b) -> Behavior (a,b)
    syncPair (Beh x, Beh y) = Beh(\t -> (x t, y t))

    polyShapeAnim :: [Behavior Coordinate] -> Behavior Region
    polyShapeAnim = lift1 (Shape . Polygon) . syncList

    spectrum :: [Color]
    spectrum = [c|c<-[minBound..], c/=Black]

    main8 :: IO()
    main8 = do animateB "Kaleido1 (close window for next demo)" kaleido1
	       animateB "Kaleido2" kaleido2

