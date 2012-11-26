{-# OPTIONS_GHC -O2 #-}
import Data.Array.Unboxed
 
main = print a 
 where
   a = foldl (+) 0 (primesToNA 2000000)
 
primesToNA n = 2: [i | i <- [3,5..n], ar ! i]
  where
    ar = f 5 $ accumArray (\ a b -> False) True (3,n) 
                        [(i,()) | i <- [9,15..n]]
    f p a | q > n = a
          | True  = if null x then a' else f (head x) a'
      where q = p*p
            a' :: UArray Int Bool
            a'= a // [(i,False) | i <- [q, q+2*p..n]]
            x = [i | i <- [p+2,p+4..n], a' ! i]
