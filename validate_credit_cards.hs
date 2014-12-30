import Data.Char (digitToInt)

toIntegerArray :: Integer ->[Int]
toIntegerArray(n)
| n < 0 =[]
| otherwise = map digitToInt(show(n))

toIntegerArrayReverse :: Integer -> [Int]
toIntegerArrayReverse  n  = reverse $ toIntegerArray $ n

doubleEverysecond :: [Int] -> [Int]
doubleEverysecond([]) = []
doubleEverysecond(x:[]) = [2*x]
doubleEverysecond(x:y:xs) = (2*x) : y:  doubleEveryOther(xs)

doubleEveryOther :: [Int] -> [Int]
doubleEveryOther(n) =  reverse $ doubleEverysecond $ reverse $ n

sumDigitsArray :: [Int] -> Int
sumDigitsArray [] = 0
sumDigitsArray(x:xs) =  sumDigits(x) + sumDigitsArray(xs)

sumDigits :: Int -> Int
sumDigits(0)  = 0
sumDigits(n)  = (mod n 10 ) + sumDigits(div n 10)


validate :: Integer -> String
validate n
|( sumDigitsArray $ doubleEveryOther $ toIntegerArrayReverse $ n) `mod` 10 == 0 = "valid"
| otherwise = "invalid"

main = do
  putStrLn "Enter credit card number"
  cc_string <- getLine
  let cc_number =  read cc_string  :: Integer
  putStrLn ("Credit card number " ++ cc_string ++ "is " ++ validate cc_number )
