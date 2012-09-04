import Data.List (nub, group, sort)
-- res = [b | b<- map triangle (map f [500..]), (length (factors b (take 1 (factors b 1)))) > 500]

triangle x = foldl (+) 0 x
f x= [1..x]

-- factors x n = [b | b<-[1..(x `quot` n)], isFactor x b]
ts = scanl1 (+) [1..]

nextTriangle x n = x + n + 1

triangles x n= [x] ++ (nextTriangle x n):[]

isFactor x n = x `rem` n == 0

factor :: Integral a => a -> a -> [a]
factor x a
  | isPrime x = [x]
  | x `mod` a /= 0 = factor x (a+1)
  | x `mod` a == 0 = (a):[] ++ factor (x `div` a) 2

isPrime k = null [ x | x <- [2..k - 1], k `mod`x  == 0]

res = [x | x<-ts, numFactors x > 500]

sift = nub res

sift2 = [x | x<-sift ]

instances x = map length (group $ sort x)

add1 x = map (+1) (instances x)

numFactors x = foldl (*) 1 (add1 $ factor x 2)