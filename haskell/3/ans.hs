main = print a
  where
      a = last $ init $ factor 600851475143 2
      
largestPrimeFactor :: Integral a => [a] -> a
largestPrimeFactor x 
  | last x == 1 = largestPrimeFactor $ init x
  | x == x = primes $ last x

factor :: Integral a => a -> a -> [a]
factor x a
  | x< 1000 = [x]
  | x `mod` a /= 0 = factor x (a+1)
  | x `mod` a == 0 = (a):[] ++ factor (x `div` a) 2

nextPrime :: Integral a => a -> a
nextPrime x
  | x == 2 = 3
  | x == 3 = 5
  | x == 5 = 7
  | (x + 1) == primes (x+1) = x+1
  | otherwise = nextPrime (x+1)

primes :: Integral a => a -> a
primes x
  | x == 2 = 2
  | x == 3 = 3
  | x == 5 = 5
  | x == 7 = 7
  | otherwise = last $ parseSieve $ sieveList x

removeT :: (a,a,a) -> a
removeT (a,b,c) = a
     
parseSieve :: Integral a => [(a,a,a)] -> [a]
parseSieve ((x,a,b):xs)
  | xs == [] = [1]
  | a == 1 = (x):[] ++ (parseSieve $ primeIt (a,xs))
  | otherwise = parseSieve xs

primeIt :: Integral a => (a,[(a,a,a)]) -> [(a,a,a)]
primeIt (_,[]) = []
primeIt (b,xs) =[x | x<-map (isMultiple b) xs]

isMultiple :: Integral a => a -> (a,a,a) -> (a,a,a)
isMultiple x (a,b,c)
  | rem x (a*a) == 0 = (a,0,c)
  | otherwise = (a,b,c)

{- Reuturn a list with -}
sieveList :: Integral a => a -> [(a,a,a)]
sieveList x = map primeSieve2 (map primeSieve (startSieve x))

prime :: Integral a => [a] -> ([a],a)
prime x = supaPrime ((rest x),(head x))

rest :: Integral a => [a] -> [a]
rest (d:xs) = [b | b<-xs, rem b d /= 0]

supaPrime :: Integral a => ([a],a) -> ([a],a)
supaPrime ([],n) = ([],n)
supaPrime ((c:x),n) = supaPrime ([b|b<-x, rem b c /= 0],c)

factors :: Integral a => a -> [a]
factors x = [b | b<- [1..x], rem x b == 0]

-- return tuples with (n,i,m) where n = number i = prime marker (0 is notprime) m is remainder with 60
startSieve :: Integral a => a -> [(a,a,a)]
startSieve x = zipWith mergeTuple[(n,i) | n <- [6..x], i <- [0]] [m|m<-map (`rem` 60) [6..x]] 

mergeTuple :: (a,a) -> a -> (a,a,a)
mergeTuple (x,y) n = (x,y,n)

-- mark all tuples with the number of solutions their to respective equations
primeSieve :: Integral a => (a,a,a) -> (a,a,a)
primeSieve (x,y,z)
  | z == 1 = (x,y,equno x)
  | z == 13 = (x,y,equno x)
  | z == 17 = (x,y,equno x) 
  | z == 29 = (x,y,equno x)
  | z == 37 = (x,y,equno x) 
  | z == 41 = (x,y,equno x) 
  | z == 49 = (x,y,equno x) 
  | z == 53 = (x,y,equno x)
  | z == 7 = (x,y,eqdos x)
  | z == 19 = (x,y,eqdos x)
  | z == 31 = (x,y,eqdos x)
  | z == 43 = (x,y,eqdos x)
  | z == 11 = (x,y,eqtrois x)
  | z == 23 = (x,y,eqtrois x)
  | z == 47 = (x,y,eqtrois x)
  | z == 59 = (x,y,eqtrois x)
  | otherwise = (x,y,-1) 

-- mark all primes
primeSieve2 :: Integral a => (a,a,a) -> (a,a,a)
primeSieve2 (x,y,z)
  | odd z && z /= -1 = (x,1,z)
  | otherwise = (x,y,z)

equno :: Integral a => a -> a
equno n = fromIntegral $ length [(x,y) | x<- [1..n], y <- [1..n], 4*x*x+y*y == n]

eqdos :: Integral a => a -> a
eqdos n = fromIntegral $ length [(x,y) | x<- [1..n], y <- [1..n], 3*x*x+y*y == n]

eqtrois :: Integral a => a -> a
eqtrois n = fromIntegral $ length [(x,y) | x<- [1..n], y <- [1..n], 3*x*x-y*y == n, x > y]

