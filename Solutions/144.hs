data Point = Point Double Double -- {x, y :: Double}
	deriving (Show)

-- a line, expressed by its slope and its y-intercept.
data Line = Line Slope Double
	deriving (Show)

type Slope = Double

-- given: a point on the ellipse
-- returns: the slope of the tangent line at that point.
getSlope :: Point -> Slope
getSlope (Point x y) = -4 * x / y

-- given: the slope (m) of the surface, the slope (a) of the laser
-- returns: the slope of the laser after being reflected off of the surface
-- derivation: use the fact that the angle of a line is equal to the arctangent
--   of its slope. this yields (tan (2*atan m - atan a)). then use trigonometric
--   identities to simplify.
reflectSlope :: Slope -> Slope -> Slope
reflectSlope m a = (a*m*m - a + 2*m) / (2*a*m + 1 - m*m)

-- given: point and slope of a line
-- returns: the equation of the line
lineEq :: Point -> Slope -> Line
lineEq (Point x1 y1) m = Line m (y1 - m*x1)

-- given: the equation of a line
-- returns: the points of intersection between that line and the ellipse.
-- derivation: substitutes (y = m*x + b) into (4*x*x + y*y = 100) and
--   uses the quadratic equation.
solve :: Line -> (Point, Point)
solve (Line m y_int) = (Point x1 (f x1), Point x2 (f x2))
	where
		a = 4 + m*m
		b = 2 * m * y_int
		c = y_int*y_int - 100
		d = sqrt $ b*b - 4*a*c
		a2 = 2 * a
		x1 = (-b+d) / a2
		x2 = (-b-d) / a2
		f x = m * x + y_int

-- given: the point that is the current position of the laser
--        the line that the laser is currently traveling on
-- returns: the same paramaters as above, but after the laser strikes
--   the next wall.
nextPos :: (Point, Line) -> (Point, Line)
nextPos ((Point x _), line@(Line m _)) = (newPoint, newLine)
	where
		(p1@(Point x1 _), p2@(Point x2 _)) = solve line
		
		-- choose the point with the x-value that is farther away from the
		-- current point. this is done so that the same point isn't returned.
		newPoint = if diff x1 x > diff x2 x
			then p1
			else p2
		
		newSlope = reflectSlope (getSlope newPoint) m
		
		newLine = lineEq newPoint newSlope

-- returns the positive difference between two Doubles
diff :: Double -> Double -> Double
diff x y = abs $ x - y

firstLine :: Line
firstLine = Line ((10.1+9.6)/(-1.4)) 10.1

firstPoint :: Point
firstPoint = Point 0 10.1

isInHole :: Point -> Bool
isInHole (Point x y) = diff x 0 <= 0.01   &&   diff y 10 <= 0.01

positions :: [(Point, Line)]
positions = iterate nextPos (firstPoint, firstLine)

main :: IO ()
main = print $ subtract 1 $ length $ takeWhile (not . isInHole . fst) positions