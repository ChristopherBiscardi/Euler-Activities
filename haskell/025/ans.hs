import Data.Char

main = print a 
  where
    a = take 5 fib

-- Fibonacci
fib = 1:1:[a+b|(a,b)<-zip fib (tail fib)] 
--Split number into array
split x = map digitToInt $ show x
--may a list such that [(fib 1, 1),(fib 2, 2)..]
l = zip fib [1,2..]
--The number returned with it's position
num = take 1 [(x,y) | (x,y) <- l, (length $ split x) >= 1000]

