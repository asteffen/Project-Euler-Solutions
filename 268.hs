import Data.Numbers.Primes (primes)

primesTo100 :: [Integer]
primesTo100 = takeWhile (< 100) primes

divides :: Integer -> Integer -> Bool
d `divides` n = n `mod` d == 0

qualifies :: Integer -> Bool
qualifies n = (>= 4) $ length $ filter (`divides` n) primesTo100