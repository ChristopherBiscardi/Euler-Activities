-- for square of side length n
grid n = div (fact (2*n)) ((fact n)*(fact n))
--factorial
fact x
  | x == 1 = x
  | otherwise = x * fact(x-1)

