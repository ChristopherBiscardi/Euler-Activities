prime :: Integral a => [a] -> [a]
prime x
  | head x == last x = [1]
  | otherwise  = (head x):[] ++ (prime $ rest x)

rest :: Integral a => [a] -> [a]
rest xs = [b | b<-xs, rem b (head xs) /= 0, b > head xs]
