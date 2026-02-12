invertLista:: [Int]->[Int]
invertLista [] = []
invertLista (a:x) = invertLista(x) ++ [a]