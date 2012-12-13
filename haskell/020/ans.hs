import Data.Char

--factorial
fact x
  | x == 1 = x
  | otherwise = x * fact(x-1)

-- split digits into individual characters
split x = map digitToInt $ show x

-- the answer
ans = sum $ split $ fact 100

-- RUN
-- ghci
-- :add ans.hs
-- ans
