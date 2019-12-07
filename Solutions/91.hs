-- 120 sec

data Point = Point Integer Integer
	deriving (Show, Eq, Ord)

origin :: Point
origin = Point 0 0

-- returns the square of the distance between the points.
squareOfDist :: Point -> Point -> Integer
squareOfDist (Point x1 y1) (Point x2 y2) = (x1 - x2)^2 + (y1 - y2)^2

-- returns (True) if the 2 points, along with the origin,
-- form the vertices of a right triangle.
formRt :: Point -> Point -> Bool
formRt p q = p /= origin && q /= origin && (a + b == c || a + c == b || b + c == a)
	where
		a = squareOfDist p origin
		b = squareOfDist q origin
		c = squareOfDist p q

{-
returns the amount of distinct right triangles that can be formed,
where the origin is one vertex and the 2 other vertices have integer
coordinates between 0 and n, inclusive.

Note: the (p < q) requirement ensures that the points are not equal,
and that opposite pairs are not counted. By opposite pairs, I mean
(a, b) and (b, a).
-}
allFormRt :: Integer -> [(Point, Point)]
allFormRt n = [(p, q) | let range = [0..n],
	x1 <- range, y1 <- range, x2 <- range, y2 <- range,
	let p = Point x1 y1, let q = Point x2 y2,
	p < q, formRt p q]

main :: IO ()
main = print $ length $ allFormRt 50