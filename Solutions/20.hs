import Data.Char (digitToInt)

factorial :: Integer -> Integer
factorial n = product [1..n]

sumOfDigits :: Integer -> Int
sumOfDigits = sum . map digitToInt . show

main :: IO ()
main = print $ sumOfDigits $ factorial 100