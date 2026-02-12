import Data.Char
{- (1)-Declare, em Haskell, as funções abaixo, contemplando, também, os protótipos (cabeçalhos):-}
f1:: Float -> Float
f1 x 
	|x >= 0 = (x+4)/(x+2)
	|x < 0 = 2/x

f2:: Int->Int->Int
f2 x y
	|x >= y = x + y
	|x < y = x - y

f3:: Int->Int->Int->Int
f3 x y z
	|(x+y) > z = (x+y+z)
	|(x+y) < z = (x-y-z)
	|(x+y) == z = 0

{- (2)-Localize, explique e corrija o erro na função que deve calcular o fatorial de um número, como se segue:
fat::Int->Int
fat x = x * fat(x-1)-}

{-Codigo corrigido-}
fat:: Int->Int
fat 0 = 1
fat x = x * fat(x-1)
{-O erro no codigo é que faltava uma base para impedindo o looping infinito, qualquer valor positivo informado seria feito até 0 e depois -1 -2 -3...-}

{- (3)-Considere a função em Haskell soma::Int->Int->Int que retorna a soma entre os dois parâmetros. Assim, faça uma função em Haskell que resulte a multiplicação de dois parâmetros
fazendo uso da função soma.-}

{-Função soma utilizada para exercicio 3-}
soma:: Int->Int->Int
soma x y = x + y

multiplicacaoS:: Int->Int->Int
multiplicacaoS x 0 = 0
multiplicacaoS x y = soma (x) (multiplicacaoS(x) (y-1))

{- (4)- Escreva, em Haskell, a função invertInt::Int->Int que inverta os dígitos de um número inteiro.
Main> invertInt 123 = 321-}

invertInt :: Int -> Int
invertInt x = aux x 0
	where
		aux 0 numInvert = numInvert {-retorna o numero invertido-}
		aux x numInvert = aux (x `div` 10) (numInvert * 10 + (x `mod` 10)) {-sempre que um número for adicionado precisamos aumentar uma dezena (*10) e somar o valor que queremos adicionar-}
		{-separa os digitos do ultimo digito 123 fica 12 / separa o ultimo digito dos outros digitos 123 fica 3-}
		{-`div` é uma divisão que ignora a parte decimal --- `mod`é uma divisão que salva o resto da dvisisão-}

{-(5) Escreva, em Haskell, a definição de uma função fourPower que retorne o seu argumento elevado à quarta potência. 
Use a função square dada em sala de aula na definição de fourPower. fourPower :: Int -> Int-}

square :: Int -> Int
square x = x * x

fourPower:: Int->Int
fourPower x = square(square(x)) 

{-(6) - -}

{-(7) - -}

{-(8) Considere a função escrita na linguagem C que calcula o máximo denominador comum entre
dois números:

int mdc(int m, int n) {
while ((m \% n) != 0) {
int aux = m;
m = n;
n = aux \% n ;
}
return n ;
}-}

mdc:: Int->Int->Int
mdc m n
	|(m `mod` n) /= 0 = mdc n (m `mod` n)
	|otherwise = n


{-(9) - Escreva, em Haskell, uma função que retorna quantos múltiplos de um determinado inteiro tem
em um intervalo fornecido. Por exemplo, o número 4 tem 2 múltiplos no intervalo de 1 a 10.
howManyMultiples 4 1 10 = 2-}

howManyMultiples:: Int->Int->Int->Int
howManyMultiples x i f
	| i > f = 0
	|(i `mod` x == 0) = 1 + howManyMultiples x (i+1) f {-Como não possui um parametro para armazenar inclui uma soma no retorno da função onde acumula os multiplos-}
	|otherwise = howManyMultiples x (i+1) f {-Se não for multiplo chama o proximo sem acumular-}

{-(10)- Escreva, em Haskell, uma função que retorna o último dígito de um número inteiro.
lastDigit 1234 = 4-}

lastDigit:: Int->Int
lastDigit x = x `mod` 10 {-O resto da divisão(`mod`) de um númeiro inteiro por 10 sempre é o número mais a direiza possivel-}

{-(11) - -}

{- (12) - . Um programador especificou a função allDiferent para identificar se três números inteiros são
todos diferentes entre si, da seguinte forma:
allDifferent::Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (n/=p)

(a) O que está errado nessa dfinição? - m != n e - n != p não quer dizer que m != p
(b) Especifique corretamente uma função allDiferent para o propósito necessário.-}

allDifferent:: Int->Int->Int->Bool
allDifferent m n p = (m/=n) && (m/=p) && (n/=p) {-É necessario conferir se o primeiro também é diferente do ultimo-}

{-(13) - Escreva uma função howManyEqual que retorne quantos dos três números inteiros fornecidos
como argumentos são iguais. A resposta poderá ser 3 (todos iguais), 2 (dois iguais e o terceiro
diferente) ou 0 (todos diferentes).
howManyEqual::Int->Int->Int->Int-}

howManyEqual:: Int->Int->Int->Int
howManyEqual x y z
	|x == y  && y == z = 3
	|x == y || x == z || y == z = 2
	|otherwise = 0





