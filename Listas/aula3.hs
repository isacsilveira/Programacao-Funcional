{- Assunto: listas e tuplas -}
periodo::Int
periodo = 7

maxi :: Int -> Int -> Int
maxi m n
   |m >= n = m
   |otherwise	= n


-- tabela de vendas
vendas :: Int -> Int
vendas 0 = 0
vendas 1 = 202
vendas 2 = 72
vendas 3 = 48
vendas 4 = 202
vendas 5 = 91
vendas 6 = 55
vendas 7 = 202

{- 01 função que retorna uma lista de vendas -}
--listaVendas :: Int-> [Int]

listaVendas:: Int->[Int]
listaVendas 0 = []
listaVendas x = (vendas x):listaVendas(x-1)

listaVenda:: Int->[Int]
listaVenda x = vendas x:[]

{- 02 função que retorna [[Int]] com listas de dia e venda -}
--listaDiaVendas :: Int-> [Int]
listaDiaVendas :: Int->[[Int]]
listaDiaVendas 0 = []
listaDiaVendas x = (x:(listaVenda x)):listaDiaVendas(x-1)

----------------------------------------------------------
{- 03 função que ordena uma lista de inteiros -}
--ordenaLista::[Int]->[Int]
ordenaLista :: [Int] -> [Int]
ordenaLista xs
  | xs == comparaElemento xs = xs
  | otherwise = ordenaLista (comparaElemento xs)
  where
      comparaElemento (x:y:b)
         |x < y = x:comparaElemento(y:b)
         |otherwise = comparaElemento(y:x:b)
      comparaElemento xs = xs

ordenaLista1:: [Int]->[Int]
ordenaLista1 (x:y:b)
   |x > y = ordenaLista1(y: ordenaLista1 (x:b) )
   |otherwise = x: ordenaLista1 (y:b)
ordenaLista1 xs = xs
-------------------------------------------------------------------------
{- 04 função que ordena [[Int]] pelo primeiro Int de cada lista  -}
--ordenaListaLista::[[Int]]->[[Int]]


---------------------------------------------------------------------------
{- 05 função que ordena as listas internas de [[Int]] e, em seguida, ordena a [[Int]] -}
--ordenaLILE::[[Int]] ->[[Int]]


-----------  tuplas --------------------------------------------------------
{- 06 função que gera uma lista de tuplas com dia e venda -}
--listaTuplaDiaVenda :: Int-> [(Int, Int)]
listaTuplaDiaVenda:: Int->[(Int,Int)]
listaTuplaDiaVenda 0 = []
listaTuplaDiaVenda n = (n, vendas n): listaTuplaDiaVenda(n-1)

{- 07 função que gera o total de vendas-}
--totalVendasT::[(Int, Int)] -> Int
totalVendasT:: [(Int,Int)]->Int
totalVendasT [] = 0
totalVendasT ((a,b):x) = b + totalVendasT x

{- 08 função que retorna a maior venda -}
--maiorVendaT::[(Int, Int)] -> Int
maiorVendaT:: Int->[(Int,Int)]->Int
maiorVendaT mv [] = mv
maiorVendaT mv ((a,b):x)
   |mv > b = maiorVendaT mv x
   |otherwise = maiorVendaT b x


{- 09 função que retorna os dias das maiores vendas -}
diasMaioresVendas::Int->[(Int,Int)]->[Int]
diasMaioresVendas _ [] = []
diasMaioresVendas mv ((a,b):x)
   |mv == b = a: diasMaioresVendas mv x
   |otherwise = diasMaioresVendas mv x
   

--diasMaioresVendas (maiorVendaT 0 (listaTuplaDiaVenda 7)) (listaTuplaDiaVenda 7)
listaDiasMaioresVendas:: [Int]
listaDiasMaioresVendas = diasMaioresVendas (maiorVendaT 0 (listaTuplaDiaVenda 7)) (listaTuplaDiaVenda 7) --Chama as funções anteriores

{-apaga a penultima ocorrencia de um numero em uma lista-}
ocorrencia:: Int->[Int]->Int
ocorrencia _ [] = 0
ocorrencia n (a:x)
   |n == a = 1 + ocorrencia n x
   |otherwise = ocorrencia n x

apagaO:: Int->Int->[Int]->[Int]
apagaO _ _ [] = []
apagaO _ _ [a] = [a]
apagaO n o (a:x)
   |n == a && o == 2 = x
   |n == a = a: apagaO n (o-1) x
   |otherwise = a: apagaO n o x

{-Chama as funções de o apagar ultima e ocorrencia para função vendas-}
apagaOcorrencia::Int->[Int]
apagaOcorrencia x = apagaO (x) (ocorrencia x (listaVendas 7)) (listaVendas 7)

