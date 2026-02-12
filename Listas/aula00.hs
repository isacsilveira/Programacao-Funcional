
periodo:: Int
periodo = 7

answer:: Int
answer = 42

soma:: Int->Int->Int
soma a b = a + b

allEquals:: Int->Int->Int->Bool
allEquals a b c = (a==b) && (b==c)

f:: Int->Int
f 1 = 17
f 2 = 5
f 3 = 13
f 4 = 32
f 5 = 1
f 6 = 33
f 7 = 12
f x = -1

maiorVenda:: Int->Int->Int
maiorVenda 0 v = v
maiorVenda d v
	|(f d) > v = maiorVenda (d-1) (f d)
	|otherwise = maiorVenda (d-1) v  

maiorV:: Int
maiorV = maiorVenda periodo 0