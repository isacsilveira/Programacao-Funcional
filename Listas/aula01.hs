import Data.Char

type Dia = Int
type Venda = Int

periodo::Int
periodo = 7

answer :: Int
answer = 42

square :: Int -> Int
square x = x * x

soma::Int->Int->Int
soma z k = z+k

allEqual :: Int -> Int -> Int -> Bool
allEqual m n p = (m==n) && (n==p)

maxi :: Int -> Int -> Int
maxi m n
   |m >= n = m
   |otherwise	= n


f :: Dia -> Venda
f 1 = 50
f 2 = 7
f 3 = 15
f 4 = 14
f 5 = 8
f 6 = 0
f 7 = 5
f x = -1

{- função que retorne a maior venda da semana -}
maiorVenda::Int->Int->Int
maiorVenda 0 v = v
maiorVenda d v
  |f d > v = maiorVenda (d-1) (f d)
  |otherwise = maiorVenda (d-1) (v)

maiorV7 :: Int
maiorV7 = maiorVenda periodo 0

{- Exercícios:
   implemente uma função que retorne o dia em que houve a maior venda (função f)-}

diaMaiorVenda:: Int->Int->Int
diaMaiorVenda 0 dmv = dmv
diaMaiorVenda d dmv
	| f dmv > f(d-1) = diaMaiorVenda (d-1) dmv
	| otherwise = diaMaiorVenda (d-1) (d-1)

   
{- implemente uma função que retorne a quantidade de vendas do período -}
totalVendas:: Int->Int
totalVendas 0 = 0
totalVendas p = f(p) + totalVendas(p-1)
{- implemente uma função que retorne a média de vendas-}

{-soma de 2 inteiros-}
s:: Int->Int->Int
s x y = x + y

{-multiplicacao com soma-}
m1:: Int->Int->Int
m1 x 0 = 0
m1 x y = x + m (x) (y-1)

{-multiplicacao funcao soma-}
m:: Int->Int->Int
m x 0 = 0
m x y = soma (x) (m (x) (y-1))
