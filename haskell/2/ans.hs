ghci:
let fibi = 1:1:[a+b|(a,b)<-zip fibi (tail fibi)]
let ans = [x | x <- fibi, x < 4000000, x `mod` 2 == 0]
sum $ take 11 ans
