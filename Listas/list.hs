import Data.Char

f1:: Int->[Int]->[Int]
f1 x l = [x * a| a <- l, a < 10] ++ [a| a <-l, a > 10]


f2 :: [(Int,Char)]   
f2 = [ (a,b) | a <-[1..27], b<-['a'..'z']]


f2:: [(Int,Char)]->[(Int,Char)]
f2 [] = []
f2 ((a,b):l)
    |a ==     = (a,b): f2 l
    |otherwise = f2 l


f3::[(Int,Char)]->[Bool]
f3 (a:l)
    |fst a == fst