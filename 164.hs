import Time

nums = [[d1,d2,d3,d4] |
	d1 <- [1..9],
	d2 <- [0..9],
	d3 <- [0..9-d1-d2],
	d4 <- [0..9-d2-d3],
	d5 <- [0..9-d3-d4],
	d6 <- [0..9-d4-d5],
	d7 <- [0..9-d5-d6],
	d8 <- [0..9-d6-d7],
	d9 <- [0..9-d7-d8],
	d10 <- [0..9-d8-d9],
	d11 <- [0..9-d9-d10]
	]

main = timePrint $ length nums