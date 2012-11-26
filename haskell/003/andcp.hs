main = print a
  where
      a = prime [2..60085]

prime :: Integral a => [a] -> [a]
prime x
  | length x == 1 = x
  | otherwise  = prime $ rest (head x) x

rest :: Integral a => a -> [a] -> [a]
rest d xs = [b | b<-xs, rem b d /= 0]
