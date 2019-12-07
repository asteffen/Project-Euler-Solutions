-- TODO: to optimize this, derive a general function that takes the
-- side lengths (a, b, c) of a triangle and returns TA + TB + TC,
-- where T is the torricelli point and A, B, C are the vertices.

data Point = Point Double Double
	deriving (Show, Eq)

-- distance between two points.
distance :: Point -> Point -> Double
distance (Point x1 y1) (Point x2 y2) = sqrt $ (x1 - x2)^2 + (y1 - y2)^2

-- midpoint of two points.
midPoint :: Point -> Point -> Point
midPoint (Point x1 y1) (Point x2 y2) = Point (avg x1 x2) (avg y1 y2)
	where avg a b = (a + b) / 2

-- adds the coordinates of 2 points together to produce a new point.
addPoints :: Point -> Point -> Point
addPoints (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- returns the intersection point of the line defined by points
-- (x1, y1) and (x2, y2) and the line defined by points (x3, y3) and (x4, y4).
intersection (Point x1 y1) (Point x2 y2) (Point x3 y3) (Point x4 y4)
	= Point x y
	where
		m12 = (y1 - y2) / (x1 - x2)
		m34 = (y3 - y4) / (x3 - x4)
		x = (m12 * x1 - m34 * x3 + y3 - y1) / (m12 - m34)
		y = m12 * (x - x1) + y1

-- given the lengths of three sides of a triangle (a, b, and c),
-- returns the coordinates of its vertices (A, B, and C).
-- A is always (0, 0). B is always (c, 0).
vertexCoords :: Double -> Double -> Double -> (Point, Point, Point)
vertexCoords a b c = (ptA, ptB, ptC)
	where
		ptA = Point 0 0
		ptB = Point c 0
		p = (b*b + c*c - a*a) / (2*c)
		q = sqrt $ b*b - p*p
		ptC = Point p q

-- returns the torricelli point of the triangle.
torricelli :: (Point, Point, Point) -> Point
torricelli (ptA@(Point ax ay), ptB@(Point bx by), ptC@(Point cx cy)) = ptT
	where
		-- constant to multiply by the side length of an equilateral triangle
		-- to get its altitude length.
		altitude = sqrt 3 / 2
		
		c = distance ptA ptB
		
		-- external point of the equilateral triangle constructed on side c.
		ptC' = addPoints (midPoint ptA ptB) (Point 0 (-altitude * c))
		
		dx = bx - cx
		dy = cy - by
		
		-- external point of the equilateral triangle constructed on side a.
		ptA' = addPoints (midPoint ptB ptC) (Point (altitude * dy) (altitude * dx))
		
		-- the torricelli point.
		ptT = intersection ptC ptC' ptA ptA'

-- given side lengths a, b, c of a triangle, returns TA + TB + TC,
-- where T is the torricelli point of the triangle.
sumTorricelli :: Double -> Double -> Double -> Double
sumTorricelli a b c = sumDistances
	where
		triangle@(ptA, ptB, ptC) = vertexCoords a b c
		ptT = torricelli triangle
		-- sumDistances = distance ptT ptA + distance ptT ptB + distance ptT ptC
		sumDistances = sum $ map (distance ptT) [ptA, ptB, ptC]

-- returns if a Double has a fractional part of 0.
isInt :: Double -> Bool
isInt = (dblEquals 0) . snd . properFraction

-- returns if (a) is within (epsilon) of (b).
dblEquals :: Double -> Double -> Bool
a `dblEquals` b = abs (a - b) < epsilon
	where epsilon = 1e-12

-- returns if a, b, and c can be the sides of a triangle.
validTriangle :: Double -> Double -> Double -> Bool
validTriangle a b c = sumGreater a b c && sumGreater a c b && sumGreater b c a
	where sumGreater x y z = x + y > z

{-
list of the torricelli triangles up to side length n.

a < b < c

to get the at least the triangles where (a+b+c < k), call (torricelliTriangles (k/2))
reasoning:
	if   c == k/2,    (maximum of c)
	then a + b > k/2.
add the equations.
	a + b + c > k/2 + k/2
	a + b + c > k.

a can't equal b, because if they are equal, then the segment from ptC to ptC'
(in torricelli function) has undefined slope.
-}
torricelliTriangles n = [(a, b, c) |
	a <- [1..n], b <- [a+1..n], c <- [b .. min n (a+b-1)],
	--a + b > c,
	let sumT = sumTorricelli a b c,
	not $ isNaN sumT,
	isInt sumT]