{-
starting this program from long hiatus from project euler.
9/28/2015

ehh, it was a somewhat interesting problem.
but it feels like homework.
i don't enjoy programming problems that much anymore.
-}

{-
most of the work for this problem is done on paper.
the first insight is that you can tile the plane with equilateral triangles,
draw a ray from a vertex C, and if it passes through another vertex C then you just
found a way for the laser to bounce and return to C.

configure the coordinate plane so that C is the origin, B is (1, 0), A is (0, 1).
you can visualize it as the usual rectangular coordinate system, it does not matter
in terms of counting the number of laser bounces. (it is a vector transformation that
preserves the relevant stuff.)

then we find out which vertices are C.
the points C are given by (x,y) = (3n+k, k) for integers n and k.

for a given point C, it is relatively easy to calculate the number of bounces.
the number of bounces is b = 2x+2y+3
b = 6n + 4k - 3

also, we are only interested in the points C = (x, y) where x and y are coprime.
because otherwise, the laser has exited through C already.

my solution starts by generating all the possible (n, k) such that 6n+4k = b+3,
where b = 12017639147. (the number given in the problem)
it filters out the points where x and y are not coprime.

the number of pairs (n, k) found is then multiplied by 2 to account for the
reverse path. this is the answer.
-}

import Time

coprime a b = gcd a b == 1


-- finding (n, k) such that b+3=6n+4k
getnk b = [(n,k) |
    n <- [0..div b3 6],
    let (k, rem) = divMod (b3-6*n) 4,
    rem == 0,
    coprime n k]
    where b3 = b + 3


{-
*Main> numways 11
2
*Main> numways 1000001
80840
-}
numways b = (*2) $ length $ getnk b


f = timePrint $ numways 12017639

main = timePrint $ numways 12017639147

{-
compiled with -O

120176391
1.9 sec

1201763914
11.6 sec

12017639147
answer = 1209002624
Computation time = 201.781250 sec

-}