import Data.List (transpose)
horiz = [[08, 02, 22, 97, 38, 15, 00, 40, 00, 75, 04, 05, 07, 78, 52, 12, 50, 77, 91, 08],[49, 49, 99, 40, 17, 81, 18, 57, 60, 87, 17, 40, 98, 43, 69, 48, 04, 56, 62, 00],[81, 49, 31, 73, 55, 79, 14, 29, 93, 71, 40, 67, 53, 88, 30, 03, 49, 13, 36, 65],[52, 70, 95, 23, 04, 60, 11, 42, 69, 24, 68, 56, 01, 32, 56, 71, 37, 02, 36, 91],[22, 31, 16, 71, 51, 67, 63, 89, 41, 92, 36, 54, 22, 40, 40, 28, 66, 33, 13, 80],[24, 47, 32, 60, 99, 03, 45, 02, 44, 75, 33, 53, 78, 36, 84, 20, 35, 17, 12, 50],[32, 98, 81, 28, 64, 23, 67, 10, 26, 38, 40, 67, 59, 54, 70, 66, 18, 38, 64, 70],[67, 26, 20, 68, 02, 62, 12, 20, 95, 63, 94, 39, 63, 08, 40, 91, 66, 49, 94, 21],[24, 55, 58, 05, 66, 73, 99, 26, 97, 17, 78, 78, 96, 83, 14, 88, 34, 89, 63, 72],[21, 36, 23, 09, 75, 00, 76, 44, 20, 45, 35, 14, 00, 61, 33, 97, 34, 31, 33, 95],[78, 17, 53, 28, 22, 75, 31, 67, 15, 94, 03, 80, 04, 62, 16, 14, 09, 53, 56, 92],[16, 39, 05, 42, 96, 35, 31, 47, 55, 58, 88, 24, 00, 17, 54, 24, 36, 29, 85, 57],[86, 56, 00, 48, 35, 71, 89, 07, 05, 44, 44, 37, 44, 60, 21, 58, 51, 54, 17, 58],[19, 80, 81, 68, 05, 94, 47, 69, 28, 73, 92, 13, 86, 52, 17, 77, 04, 89, 55, 40],[04, 52, 08, 83, 97, 35, 99, 16, 07, 97, 57, 32, 16, 26, 26, 79, 33, 27, 98, 66],[88, 36, 68, 87, 57, 62, 20, 72, 03, 46, 33, 67, 46, 55, 12, 32, 63, 93, 53, 69],[04, 42, 16, 73, 38, 25, 39, 11, 24, 94, 72, 18, 08, 46, 29, 32, 40, 62, 76, 36],[20, 69, 36, 41, 72, 30, 23, 88, 34, 62, 99, 69, 82, 67, 59, 85, 74, 04, 36, 16],[20, 73, 35, 29, 78, 31, 90, 01, 74, 31, 49, 71, 48, 86, 81, 16, 23, 57, 05, 54],[01, 70, 54, 71, 83, 51, 54, 69, 16, 92, 33, 48, 61, 43, 52, 01, 89, 19, 67, 48]]
vert = transpose horiz
tlbr = startDiag horiz
trbl = startDiag $ reverse horiz
horizResults = concat $ map calcProds horiz
vertResults = concat $ map calcProds vert
tlbrResults = concat $ map calcProds tlbr
trblResults = concat $ map calcProds trbl

results = maximum $ (horizResults ++ vertResults ++ tlbrResults ++ trblResults)

calcProds :: Integral a => [a] -> [a]
calcProds x
  | x == [] = x
  | length x < 4 = calcProds $ tail x
  | otherwise = (foldl (*) 1 (take 4 x)):[] ++ calcProds (tail x)

makeDiag :: Integral a => [[a]] -> [[a]]
makeDiag x
  | (length x) == 1 = [(head $ last x):[]]
  | last x == [] = makeDiag (init x)
  | (length $ head x) < 20 = (map head (reverse x)):[] ++ makeDiag (map tail x)
  | (length $ last x) == 20 = map (head . last) (apxs 0 x):[] ++ passNextDiag 1 0 x
  | (length $ last x) == 19 = map (head . last) (apxs 1 x):[] ++ passNextDiag 2 1 x
  | (length $ last x) == 18 = map (head . last) (apxs 2 x):[] ++ passNextDiag 3 2 x
  | (length $ last x) == 17 = map (head . last) (apxs 3 x):[] ++ passNextDiag 4 3 x
  | (length $ last x) == 16 = map (head . last) (apxs 4 x):[] ++ passNextDiag 5 4 x
  | (length $ last x) == 15 = map (head . last) (apxs 5 x):[] ++ passNextDiag 6 5 x
  | (length $ last x) == 14 = map (head . last) (apxs 6 x):[] ++ passNextDiag 7 6 x
  | (length $ last x) == 13 = map (head . last) (apxs 7 x):[] ++ passNextDiag 8 7 x
  | (length $ last x) == 12 = map (head . last) (apxs 8 x):[] ++ passNextDiag 9 8 x
  | (length $ last x) == 11 = map (head . last) (apxs 9 x):[] ++ passNextDiag 10 9 x
  | (length $ last x) == 10 = map (head . last) (apxs 10 x):[] ++ passNextDiag 11 10 x
  | (length $ last x) == 9 = map (head . last) (apxs 11 x):[] ++ passNextDiag 12 11 x
  | (length $ last x) == 8 = map (head . last) (apxs 12 x):[] ++ passNextDiag 13 12 x
  | (length $ last x) == 7 = map (head . last) (apxs 13 x):[] ++ passNextDiag 14 13 x
  | (length $ last x) == 6 = map (head . last) (apxs 14 x):[] ++ passNextDiag 15 14 x
  | (length $ last x) == 5 = map (head . last) (apxs 15 x):[] ++ passNextDiag 16 15 x
  | (length $ last x) == 4 = map (head . last) (apxs 16 x):[] ++ passNextDiag 17 16 x
  | (length $ last x) == 3 = map (head . last) (apxs 17 x):[] ++ passNextDiag 18 17 x
  | (length $ last x) == 2 = map (head . last) (apxs 18 x):[] ++ passNextDiag 19 18 x
  | (length $ last x) == 1 = map (head . last) (apxs 19 x):[] ++ passNextDiag 20 19 x
  | otherwise = (head x):[]

startDiag x = advMakeDiag x (fromIntegral $ length $ head x)

advMakeDiag :: Integral a => [[a]] -> a -> [[a]]
advMakeDiag x n
  | head x == last x = x
  | last x == [] = advMakeDiag (init x) n
  | (testx x) < 20 = (map head (reverse x)):[] ++ advMakeDiag (map tail x) n
  | (testx x) == n = (mapx x a) ++ (advPassNextDiag (a + 1) a x n)
    where a = fromIntegral (n - (fromIntegral $ length $ last x))

mapx x n = map (head . last) (apxs n x):[]

testx x = fromIntegral $ length $ head x

advPassNextDiag n nn x nnn = advMakeDiag ((mf n x) ++ (map (tail . last) (reverse (apxs nn x)))) nnn

-- Flesh out multi-function 


mapfx n = map mf [0..n]

mf x = foldr (.) id (replicate x init)

--seek to replace [x] ++ [init x] ... with list comphrehension
--Then associate number of list items with length of last list.. or something

apx n x = [b m| b<-n,m<-[x]]

apxs n x 
  | n == 0 = [x]
  | otherwise = apx (mapfx n) x

passNextDiag n nn x= makeDiag ((mf n x) ++ (map (tail . last) (reverse (apxs nn x))))

