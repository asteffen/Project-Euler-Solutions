square :: (Num a) => a -> a
square = (^2)

sumOfSquares :: (Num a) => [a] -> a
sumOfSquares = sum . map square

squareOfSum :: (Num a) => [a] -> a
squareOfSum = square . sum

main :: IO ()
main = putStrLn $ show $ squareOfSum n100 - sumOfSquares n100
	where n100 = [1..100]