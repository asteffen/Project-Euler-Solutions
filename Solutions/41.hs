import Data.List (permutations)
import Data.Char (intToDigit)

-- Prime numbers, as an infinite list.
primes :: [Integer]
primes = 2 : filter is_prime [3,5..]

-- Mutually recursive with the primes list.
is_prime :: Integer -> Bool
is_prime p = all (\n -> p `mod` n /= 0) $ takeWhile (\n -> n * n <= p) primes

-- converts a list of digits back into an integer.
digitsToInt :: [Int] -> Integer
digitsToInt = read . map intToDigit

-- by experimentation, the largest pandigital primes has 7 digits.
pandigPrimes = filter is_prime $ map digitsToInt $ permutations [1..7]

main = print $ maximum pandigPrimes