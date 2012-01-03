module GeometryUtils where
	
	type Line = (Point2D, Point2D)
	type Point2D = (Float,Float)

	distance :: Line -> Point2D -> Float
	distance ((xA,yA), (xB,yB)) (xP,yP) = let xAB = xB - xA
				                  xAP = xP - xA
						  yAB = yB - yA
						  yAP = yP - yA
					      in abs (xAP*yAB - xAB*yAP) / (sqrt (xAB^2 + yAB^2))
