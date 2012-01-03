module Chapter5 where

    -- --------------
    -- Exercício 5.2a
    -- --------------
    {-
    map map [x] = [map x]
    Como x é o primeiro argumento de map (uma função), então x :: a -> b
    Mas map :: (a -> b) -> [a] -> [b].
    Logo map x :: [a] -> [b].
    Então map map [x] = [[a] -> [b]]
    Resposta: map map :: [a -> b] -> [[a] -> [b]]
    -}
    
    func1 :: Int -> Int
    func1 n = 2 * n
    
    func2 :: Int -> Int
    func2 n = 3 * n
    
    func3 :: Int -> Int
    func3 n = 4 * n
    
    lista :: [Int -> Int]
    lista = [func1, func2, func3]
    
    input :: [Int]
    input = [10, 20, 30, 40]

    -- output = [func1, func2, func3]
    output = map map lista -- O que fazer com isso?????

    funcao1 = head output
    int1 = funcao1 input -- Executa a func1 em todos os elementos de input

    funcao2 = head (tail output)
    int2 = funcao2 input -- Executa a func2 em todos os elementos de input
    
    funcao3 = head (tail (tail output))
    int3 = funcao3 input -- Executa a func3 em todos os elementos de input

    -- Resumindo: se fn :: a -> b (n = 1, 2, 3 ...) e fns = [fn], então a instrução
    -- "map map fns" cria um vetor de funções que faz a mesma coisa que fn, mas que
    -- recebe como argumento uma lista de [a] e retorna uma lista de [b]. Ou seja,
    -- se eu tiver uma função fn, que só funciona para Int -> Int, por exemplo, mas
    -- gostaria de aplicá-la em todos os elementos de um vetor [Int], posso usar
    -- "map map [fn] [Int]". Isto, em particular, seria o mesmo que fazer apenas
    -- "map fn [Int]" se eu só tiver uma função fn. Mas se eu tiver mais de uma,
    -- pode ser útil ter uma regra assim à disposição. Mas ainda não está realmente
    -- claro se isto seria uma vantagem.
    
    -- --------------
    -- Exercício 5.2b
    -- --------------
    {-
    foldl foldl init [x] <- Completando os argumentos que faltam: init e [x]
    Assim, o segundo "foldl" define a operação que será feita com entre os elementos
    foldl foldl init [x1:x2:...] = init `foldl` x1 `foldl` x2 `foldl` x3 ...
    Mas foldl :: (a -> b -> a) -> a -> [b] -> a 
    Descobri, vendo na Internet, que foldl foldl dá erro.
    -}
    
    -- --------------
    -- Exercício 5.2c
    -- --------------
    {-
    map foldl [x] = [foldl x]
    Mas foldl :: [a -> b -> a] -> a -> [b] -> a
    Portanto x :: [a -> b -> a]
    De modo que [foldl x] = [a -> [b] -> a]
    Logo map foldl :: [a -> b -> a] -> [a -> [b] -> a]
    -}

    -- -------------
    -- Exercício 5.3
    -- -------------

    -- Versão recursiva
    recursiveLength :: [a] -> Int
    recursiveLength [] = 0
    recursiveLength (x:xs) = 1 + recursiveLength xs

    -- Versão com função de ordem superior
    higherOrderLength :: [a] -> Int
    higherOrderLength xs = foldl (+) 0 (map (\x -> 1) xs)

    -- Solução do livro
    -- A sacada é que a função usada por foldl recebe DOIS argumentos, sendo o primeiro
    -- o acumulador e o segundo, o elemento da lista. Bastou, então, ignorar o segundo
    -- argumento. Certamente esta solução é mais rápida que a minha, pois não precisa
    -- criar uma lista de 1s, que é o que faz "map (\x -> 1) xs"
    bookLength :: [a] -> Int
    bookLength xs = foldl (\x _ -> x + 1) 0 xs

    myList :: [Int]
    myList = [1,2,10,4,9,6,8,7,5,3]

    -- -------------
    -- Exercício 5.4
    -- -------------
    {-
    f1 :: ? -> Int -> [Int]
    f2 :: (Int -> Int -> Int) -> [Int] -> ?

    A ideia é simples: f1 multiplica cada item de uma lista por alguma coisa, que é definida por f2
    Ou seja, f2 () 5 diz que 5 é o fator a ser utilizado na função (),

    map :: (a -> b) -> [a] -> [b]
    zip :: [a] -> [b] -> [(a,b)]
    fold :: (a -> b -> b) -> b -> [a] -> b
    -}
    -- -------------
    -- Exercício 5.5
    -- -------------
    doubleEach :: [Int] -> [Int]
    doubleEach xs = map (\x -> 2 * x) xs
                    -- map doubles xs
                    -- where doubles :: Int -> Int
                    --       doubles x = 2 * x

    pairAndOne :: [Int] -> [(Int,Int)]
    pairAndOne xs = zip xs (map (\x -> x + 1) xs)

    addEachPair :: [(Int,Int)] -> [Int]
    addEachPair xs = map (\(x,y) -> x + y) xs
                     -- map addPair xs
                     -- where addPair :: (Int,Int) -> Int
                     --       addPair (x,y) = x + y

    myList2 :: [(Int,Int)]
    myList2 = [(1,2),(3,4),(5,6),(7,8),(9,10)]

    -- -------------
    -- Exercício 5.6
    -- -------------
    maxList :: [Int] -> Int
    maxList (x:xs) = foldl myMax x (x:xs)
                     where myMax :: Int -> Int -> Int
                           myMax a b
                             |a >= b    = a
                             |otherwise = b

    minList :: [Int] -> Int
    minList (x:xs) = foldl myMin x (x:xs)
                     where myMin :: Int -> Int -> Int
                           myMin a b
                             |a <= b    = a
                             |otherwise = b

    -- -------------
    -- Exercício 5.7
    -- -------------
    addPairsPointwise :: [(Int,Int)] -> (Int,Int)
    addPairsPointwise (x:xs) = foldl addPair x xs
                               where addPair :: (Int,Int) -> (Int,Int) -> (Int,Int)
                                     addPair (x1,y1) (x2,y2) = (x1+x2,y1+y2)

    -- Solução do livro (usa uma função não apresentada: unzip)
    addPairsPointwise' :: [(Int,Int)] -> (Int, Int)
    addPairsPointwise' xs = (foldl (+) 0 as, foldl (+) 0 bs)
                            where (as,bs) = unzip xs

    myList3 :: [(Int,Int)]
    myList3 = [(1,2),(3,4),(5,6)]

    -- -------------
    -- Exercício 5.8
    -- -------------
    encrypt :: String -> String
    encrypt cs = map shiftChar cs
                 where shiftChar :: Char -> Char
                       shiftChar c =
                         if sc == 256 then toEnum 0 else toEnum sc
                            where sc = 1 + (fromEnum c)

    decrypt :: String -> String
    decrypt cs = map unshiftChar cs
                 where unshiftChar :: Char -> Char
                       unshiftChar c = if uc == -1 then toEnum 255 else toEnum uc
                                       where uc = (fromEnum c) - 1

    -- -------------
    -- Exercício 5.9
    -- -------------

    -- Procedimento para resolver: pega o valor total e divide pela maior moeda, pegando o quociente
    -- Isto dá a quantidade de moedas daquele tipo. Subtrair do total o quociente * valor da moeda
    -- e refazer o cálculo, até chegar num resultado onde o quociente é zero. Neste caso,
    -- considerar a diferença como a quantidade de moedas de 1 centavo. Completar o restante das posiçãos
    -- não utilizadas com zero.

    makeChange :: Int -> [Int] -> [Int]
    makeChange amount (c:[]) = [amount]
    makeChange amount (c:cs) = let a = amount `div` c
                               in [a] ++ makeChange (amount - a * c) cs

    {-
      Simulando:
      makeChange 99 [5,1]
      makeChange 99 (5:[1]) = [19] ++ makeChange (99 - 19 * 5) [1]  <-- a = 99 `div` 5 = 19
                            = [19] ++ makeChange (99 - 19 * 5) [1]
                            = [19] ++ makeChange 4 [1]
                            = [19] ++ makeChange 4 (1:[])
                            = [19] ++ [1]
                            = [19,1]

     Outro exemplo:
     makeChange 137 [20,10,5,1]
     makeChange 137 (20:[10,5,1]) = [6] ++ makeChange (137 - 6 * 20) [10,5,1] <-- a = 137 `div` 20 = 6
                                  = [6] ++ makeChange 17 [10,5,1]
                                  = [6] ++ makeChange 17 (10:[5,1])
                                  = [6] ++ [1] ++ makeChange (17 - 1 * 10) [5,1] <-- a = 17 `div` 10 = 1
                                  = [6] ++ [1] ++ makeChange 7 [5,1]
                                  = [6] ++ [1] ++ makeChange 7 (5:[1])
                                  = [6] ++ [1] ++ [1] ++ makeChange (7 - 1 * 5) [1] <-- a = 7 `div` 5 = 1
                                  = [6] ++ [1] ++ [1] ++ makeChange 2 [1]
                                  = [6] ++ [1] ++ [1] ++ makeChange 2 (1:[])
                                  = [6] ++ [1] ++ [1] ++ [2]
                                  = [6,1,1,2]
    Ou seja, 137 = 6 * 20 + 1 * 10 + 1 * 5 + 2 * 1
    Para verificar isso, posso usar zipWith e foldl:

    137 = foldl (+) 0 (zipWith (*) [20,10,5,1] [6,1,1,2])

    Exemplo importante (quando há zeros no meio do caminho):
    makeChange 202 [20,10,5,1]
    makeChange 202 (20:[10,5,1]) = [10] ++ makeChange (202 - 10 * 20) [10,5,1] <-- a = 202 `div` 20 = 10
                                 = [10] ++ makeChange 2 [10,5,1]
                                 = [10] ++ makeChange 2 (10:[5,1])
                                 = [10] ++ [0] ++ makeChange (2 - 0*10) [5,1] <-- a = 2 `div` 10 = 0
                                 = [10] ++ [0] ++ makeChange 2 (5:[1])
                                 = [10] ++ [0] ++ [0] ++ makeChange (2 - 0*5) [1] <-- a = 2 `div` 5 = 0
                                 = [10] ++ [0] ++ [0] ++ makeChange 2 (1:[])
                                 = [10] ++ [0] ++ [0] ++ [2]
                                 = [10,0,0,2]

    Para que esta algoritmo funcione, eu preciso garantir que
    1) A lista de moedas está em ordem decrescente - newcoins = reverse (sort coins)
    2) Que o menor valor da lista é 1 - if (last newcoins > 1) then newcoins ++ [1] else newcoins
    4) Nenhum item da lista é repetido

    O jeito mais fácil de fazer isso é:
    a) Acrescento 1
    b) Removo todos os itens menores que 1
    c) Ordeno a lista
    d) Removo todos os itens repetidos

    coins' = coins ++ [1]
    coins'' = filter (\x -> x >= 1) coins'
    coins''' = reverse (sort coins'')
    coins'''' = nub coins'''
    Ou seja, dada uma lista "coins" de moedas, usarei a lista
    nub (reverse (sort (filter (\x -> x >= 1) (coins ++ [1]))))

    -}

    coins :: [Int]
    coins = [50,20,10,5,1]

    -- Verifica a resposta dada por makeChange (colocar em "change")
    checkChange :: Int -> [Int] -> [Int] -> Bool
    checkChange amount coins change = amount == foldl (+) 0 (zipWith (*) coins change)

    -- Encontrei esta solução na Internet:
    makeChange' :: Int -> [Int] -> [Int]
    makeChange' amount coins = zipWith div aux coins
                               where aux = scanl mod amount coins


    {- Entendendo (no exemplo abaixo)...
       A função scanl é similar a foldl, só que ela dá uma lista com os resultados parciais.
       No caso, ao aplicar a função "mod" (resto), começando com 202, em cada item das moedas disponíveis,
       e reaplicando o resultado, a conta que está sendo feita é justamente isso:
       Divide 202 por 20
       Pega o resto dessa divisão (o que sobrou), 2, e divide por 10, pegando novamente o resto
       Como 10 não divide 2, continua sobrando 2 (resto = 0). O mesmo vale para 5, e até chegar em 1.
       Neste caso, a divisão é exata e o resta é zero (não sobrou nada).

       Depois, ao usar "zipWith", o que se faz é justamente dividir o montante que falta, em cada
       passo, pelo valor da moeda daquele passo.

    makeChange' 202 [20,10,5,1]
      => aux = scanl mod 202 [20,10,5,1]
             = [202, 202 `mod` 20, ...]
             = [202, 2, 2 `mod` 10, ...]
             = [202, 2, 2, 2 `mod` 5, ...]
             = [202, 2, 2, 2, 2 `mod` 1]
             = [202, 2, 2, 2, 0]
    zipWith div [202, 2, 2, 2, 0] [20, 10, 5, 1]
    [202 `div` 20, 2 `div` 10, 2 `div` 5, 2 `div` 1]
    [10, 0, 0, 2]
    -}














