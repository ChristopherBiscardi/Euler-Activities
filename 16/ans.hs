import Data.Char

num = 2^1000
ans = foldr (+) 0 (split num)

split x = map digitToInt $ show x
