import Data.List(union)

--f = nL (ansx 1) (cx x 2)

ans x  =             nL (cx x 0) (cx x 1)
ans2 x =         nL (nL (cx x 0) (cx x 1)) (cx x 2)
ans3 x =     nL (nL (nL (cx x 0) (cx x 1)) (cx x 2)) (cx x 3)
ans4 x = nL (nL (nL (nL (cx x 0) (cx x 1)) (cx x 2)) (cx x 3)) (cx x 4)

-- ansx 2 3 gives the results for 2x2 grid
ansx x n
  | n == 1 = nL (cx x 0) (cx x 1)
  | otherwise = nL (ansx x (n-1)) (cx x n) 

nL n x
  | length n == 1 = map (union $ head n) x
  | otherwise = map (union $ head n) x-- ++ nL x (tail n)


co x = headList $ head $ combos x
c2 x = headList $ head $ tail $ combos x
c3 x = headList $ head $ tail $ tail $ combos x
c4 x = headList $ head $ tail $ tail $ tail $ combos x
c5 x = headList $ head $ tail $ tail $ tail $ tail $ combos x

mf x = foldr (.) id (replicate x tail)
cx x n = headList $ head $ (mf n) $ combos x


nextList x = map (union $ head $ headList $ head $ combos 4) (headList $ head $ tail $ combos 4)

startList x = headList $ head $ combos x 

headList x
  | length x == 1 = [(head x):[]]
  | otherwise = [(head x):[]] ++ (headList $ tail x)

combos n = map (combo n) [1..(n*n)]

combo n x
  | x == 1 = [x+1,x+n]
  | x == n = [x-1, x+x]
  | x < n = [x-1, x+n, x+1]
  | x == n*n-n+1 = [x-n, x+1]
  | x == n*n = [x-n, x-1] 
  | mod (x - 1) n == 0 = [x-n,x+1,x+n]
  | mod x n == 0 = [x-n, x-1, x+n] 
  | x < n*n && x > n*n-n+1 = [x-1, x-n, x+1] 
  | otherwise = [x-n, x+n, x-1, x+1] 

filterByLength n = filter (\x -> length x == n)
