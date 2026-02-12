import Data.Char

f1::Int->[Int]->Bool
f1 x [] = False
f1 x (a:b) = x == a || f1 x b

f2::Int->[[Int]]->[(Bool,[Int])]
f2 x [] = []
f2 x (a:b) = (f1 x a, a): f2 x b

f3::Int->[[Int]]->(Int,[(Bool,[Int])])
f3 x b = (x,(f2 x b))

f4::(Int,[(Bool,[Int])])->[[Int]]
f4 (x, []) = []
f4 (x,((a,b):xs))
    |(not a) = b:f4 (x,xs)
    |otherwise = f4 (x,xs)

f5::String->String->[Bool]
f5 [] _ = []
f5 _ [] = []
f5 (a:x) (b:y) = (a==b): f5 x y
