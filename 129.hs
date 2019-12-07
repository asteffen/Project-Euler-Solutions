import Time
import Data.List
import Euler
import Data.Numbers.Primes

lim = 10^5

r :: Integer -> Integer
r n = read $ genericReplicate n '1'

rs :: [Integer]
rs = iterate (\n -> 10*n + 1) 1

rs2 = map r $ 1 : primes

a :: Integer -> Int
a n = (+1) $ head $ findIndices ((== 0) . (`mod` n)) rs

-- integers coprime to 10
coprimes :: [Integer]
coprimes = [1, 3, 7, 9] ++ map (+10) coprimes

aVals :: [(Integer, Int)]
aVals = map (\n -> (n, a n)) $ map (+fromIntegral lim) coprimes

--main = timePrint $ all (\(a,b) -> fromIntegral a >= b) $ take (10^4) aVals
main = timePrint $ head $ filter ((> lim) . snd) aVals

{-
conjecture: for all n,
n >= a n

given: a n > 10^6

n > 10^6




(1017,1008)
Computation time = 0.046875 sec

(10007,10006)
Computation time = 31.453125 sec

-- moved by starting pt to 10^4

(10007,10006)
Computation time = 0.171875 sec


--- daniel.is.fischer

p divides R(10^9) iff A(p) divides 10^9

Faster than computing A(p) is using the fact that A(p) divides 10^9 iff
10^(gcd(10^9,p-1)) `mod` p == 1. 

---- prob 130

for all primes p>5, a(p) divides p-1.
-}