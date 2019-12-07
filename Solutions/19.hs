type Year = Int

data Month = January | February | March | April | May | June | July | August |
	September | October | November | December
	deriving (Show, Eq, Enum)

type Day = Int

data DayOfWeek = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
	deriving (Show, Eq, Enum)

data Date = Date {y :: Year, m :: Month, d :: Day}
	deriving (Show, Eq)

isLeapYear :: Year -> Bool
isLeapYear y
	| y `mod` 400 == 0 = True
	| y `mod` 100 == 0 = False
	| y `mod` 4   == 0 = True
	| otherwise        = False

daysInMonth :: Year -> Month -> Int
daysInMonth y m = case m of
	September -> 30
	April     -> 30
	June      -> 30
	November  -> 30
	January   -> 31
	March     -> 31
	May       -> 31
	July      -> 31
	August    -> 31
	October   -> 31
	December  -> 31
	February  -> if isLeapYear y then 29 else 28

-- calculates the day of the week of a date.
-- http://en.wikipedia.org/wiki/Calculating_the_day_of_the_week#An_algorithm_to_calculate_the_day_of_the_week
dayOfWeek :: Date -> DayOfWeek
dayOfWeek (Date y m d) = toEnum $ (century + year + month + day) `mod` 7
	where
		leap :: Bool
		leap = isLeapYear y
		
		first2y, last2y, century, year, month, day :: Int
		(first2y, last2y) = y `divMod` 100
		century = 2 * (3 - first2y `mod` 4)
		year = last2y + last2y `div` 4
		month = case m of
			January   -> if leap then 6 else 0
			February  -> if leap then 2 else 3
			March     -> 3
			April     -> 6
			May       -> 1
			June      -> 4
			July      -> 6
			August    -> 2
			September -> 5
			October   -> 0
			November  -> 3
			December  -> 5
		day = fromEnum d

-- returns the day after a given date.
tomorrow :: Date -> Date
tomorrow (Date y m d) = Date y' m' d'
	where
		isNextYear, isNextMonth :: Bool
		isNextYear  = m == December && d == 31
		isNextMonth = d == daysInMonth y m
		
		y' = if isNextYear
			then succ y
			else y
		m' = if isNextYear
			then January
			else if isNextMonth
				then succ m
				else m
		-- isNextYear is included in isNextMonth
		d' = if isNextMonth
			then 1
			else succ d

-- takes elements of a list up to the element that satisfies (p)
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p (x:xs)
	| p x       = [x]
	| otherwise = x : takeUntil p xs

-- returns the range of dates between two dates, inclusive.
datesFromTo :: Date -> Date -> [Date]
datesFromTo start_date end_date = takeUntil (== end_date) $ iterate tomorrow start_date

main :: IO ()
main = print $ length $ filter qualifies century20
	where
		-- the whole 20th century, as a list of dates
		century20 :: [Date]
		century20 = datesFromTo (Date 1901 January 1) (Date 2000 December 31)
		
		-- returns if a date is the first of a month and is a sunday
		qualifies :: Date -> Bool
		qualifies date = d date == 1 && dayOfWeek date == Sunday