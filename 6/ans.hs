sumofsquares = foldl (+) 0 [x*x | x<-[1..100]]
sums = foldl (+) 0 [1..100]
answer = sums*sums - sumofsquares
