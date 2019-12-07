-- takes about 22 seconds to run

import Data.List (permutations, sort)

main :: IO ()
main = print $ (sort . permutations) [0..9] !! 999999