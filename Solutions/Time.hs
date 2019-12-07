-- Time.hs: report the time taken to evaluate an expression.

{-
There are two timing functions included in this module.

(time): Works on instances of (NFData). Does not print the result of the expression.
(timePrint): Works on instances of (Show). Prints the result of the expression.

Modified from http://www.haskell.org/haskellwiki/Timing_computations

Note: The (time) function in the above link for timing pure computations does
not seem to work. It appears it just adds 1 to the numerical argument 1 million
times.
-}



module Time where

import Text.Printf (printf)
import System.CPUTime (getCPUTime)
import Control.Parallel.Strategies (NFData)

import Control.DeepSeq (rnf)


-- My own timing function. Uses (rnf) from (Control.Parallel.Strategies).
--time :: (NFData a) => a -> IO ()
time x = do
    start <- getCPUTime
    -- Need to print something here in order to force evaluation of (rnf x).
    rnf x `seq` putStr "Done. "
    end <- getCPUTime
    let diff = (fromIntegral $ end - start) / 1e12
    printf "Computation time = %0.6f sec\n" (diff :: Double)

-- Prints the calculation time of an expression and the return value.
timePrint :: (Show a) => a -> IO ()
timePrint x = do
    start <- getCPUTime
    print x
    end <- getCPUTime
    let diff = (fromIntegral $ end - start) / 1e12
    printf "Computation time = %0.6f sec\n" (diff :: Double)

-- Times an IO computation.
timeIO :: IO a -> IO a
timeIO x = do
    start <- getCPUTime
    v <- x
    end <- getCPUTime
    let diff = (fromIntegral $ end - start) / 1e12
    printf "Computation time = %0.6f sec\n" (diff :: Double)
    return v