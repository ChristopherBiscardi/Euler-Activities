import Data.List

next x
  | even x = x `div` 2
  | otherwise = 3 * x + 1

chain x n
  | x == 1 = n+1
  | otherwise = chain (next x) (n + 1)

--fx x n
--  | snd n > chain (head x) 0 
--  |
 
fxs x y
  | snd x > snd y = GT
  | snd x < snd y = LT
  | otherwise = LT

ans = last $ sortBy fxs nums

nums = [(x,chain x 0)| x<-[1..1000000]]

hxn x n
  | snd x > snd n = x
  | otherwise = n

stuff x
  | length x == 2 = hxn (head x) (head $ tail x)
  | otherwise = hxn (head x) (head $ tail x)

maxStuff = maximum $ map snd nums

