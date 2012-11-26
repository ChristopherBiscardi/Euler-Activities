highNum = 998001
highPal = 997799
palStart = 997

answer = findPal palStart

testPal :: Integral a => a -> [(a,a)]
testPal p = [(x,y)|x<-[1..999],y<-[1..999],x*y == p]

fromDigits :: Integral a => a -> a -> a
fromDigits num d = 10*num + d

revNum :: Integral a => a -> [a]
revNum 0 = []
revNum x = revNum (div x 10) ++ (mod x 10):[]

makePal :: Integral a => a -> a
makePal x = foldl fromDigits 0 ((revNum x) ++ (reverse $ revNum x))

findPal :: Integral a => a -> a
findPal a
  | (length $ testPal $ makePal a) > 0 = makePal a
  | otherwise = findPal (a-1)
