import Data.List

l = length $ nub $ sort $ [a^b | a <- [2..100], b <- [2..100]]

