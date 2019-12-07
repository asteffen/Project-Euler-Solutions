import Time
import Euler (sieveList)

lim = 2000000

ps :: [Integer]
ps = map fromIntegral $ takeWhile (< lim) $ sieveList $ ceiling $ sqrt $ fromIntegral lim

main = timePrint $ sum ps