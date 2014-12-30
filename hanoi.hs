type Peg = String
hanoi :: Integer -> Peg -> Peg -> Peg -> [(Peg, Peg)]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
main = do
  putStrLn "Enter Number of discs"
  disc <- getLine
  let number_of_discs =  read disc  :: Integer
  print(hanoi number_of_discs "a" "b" "c")
