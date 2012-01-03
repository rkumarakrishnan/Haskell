module Chapter11 where

    -- Exercício 11.1 (p.140)
    -- ----------------------
    
    {-
        a) Provar que putcharList cs = map putChar cs (1)

        Do capítulo 5,
        putCharList :: [Char] -> [IO ()]
        putCharList [] = []
        putChatList (c:cs) = putChar c : putCharList cs

        Caso-base: a lista vazia
        putCharList [] = []
                       = map putChar []

        Caso c:cs, supondo que vale (1):
        putCharList (c:cs) = putChar c : pucCharList cs
                           = putChar c : map putChar cs
                           = map putChar (c:cs)

        E portanto (1) é verdadeiro.



        b) Provar que listProd xs = fold (*) 1 xs    (2)

        Do capítulo 5,
        listProd xs :: [Float] -> Float
        listProd [] = 1
        listProd (x:xs) = x * listProd xs

        Caso base: a lista vazia:
        listProd [] = 1
                    = fold (*) 1 []

        Caso x:xs, supondo que vale (2):
        listProd (x:xs) = x * listProd xs
                        = x * fold (*) 1 xs
                        = (*) x (fold (*) 1 xs)
                        = fold (*) 1 (x:xs)

        E portanto (2) é verdadeiro.

     -}

    -- Exercício 11.2
    -- --------------
    {-
        a) Provar que map f = g, onde   (3)
           f :: a -> a
           f x = x

           g :: [a] -> [b]
           g [] = []
           g xs = xs


        Caso base:
            map f [] = []
                     = g []

        Caso x:xs
            map f (x:xs) =  f x : map f xs
                         =  x : g xs
                         =  x : xs
                         =  g (x.xs)
        
        E assim fica provado por indução.

        -----

        b) Provar que map (f.g) = map f . map g
            f :: a -> b
            g :: c -> a

        map (f.g) [] = []
                     = map f . map g []

        Note que usei o seguinte fato:
        map f . map g [] = map f (map g []) = map f [] = []

        map (f.g) (x:xs) = (f.g) x : map (f.g) xs
                         = (f.g) x : (map f . map g xs)
                         = f (g x) : map f (map g xs)
                         = map f ((g x) : (map g xs))
                         = map f (map g (x:xs))
                         = map f . map g (x:xs)

        E assim fica provado por indução.

        -----

        c) Provar que map f . tail = tail . map f
            f :: a -> b

            tail :: [a] -> [a]
            tail [] = []
            tail (x:xs) = xs

        map f . tail [] = map f (tail [])
                        = map f []
                        = []

        map f . tail (x:xs) = map f (tail (x:xs))
                            = map f xs
                            = tail (x : (map f xs))     <-- Acrescentei x no começo da lista e removi com tail
                            = tail (map f (x:xs))
                            = tail . map f (x:xs)

        E assim fica provado por indução.

        -----
        
        d) Provar que map f . reverse = reverse . map f

        map f . reverse [] = map f (reverse [])
                           = map f []
                           = []
                           = reverse . map f

        Pois
        reverse . map f [] = reverse (map f []) = reverse [] = []


        map f . reverse (x:xs) = map f (reverse (x:xs))
                               = map f (reverse xs : x)
                               = map f (reverse xs) : f x
                               = map f . reverse xs : f x
                               = reverse . map f xs : f x
                               = reverse (map f xs) : f x
                               = reverse (f x : map f xs)  <-- Usei reverse (x:xs) = reverse xs : x
                               = reverse (map f (x:xs))    <-- Usei map f (x:xs) = f x : map f xs
                               = reverse . map f (x:xs)

        E assim fica provado por indução.

        -----

        e) Provar que map f . concat = concat . map (map f)

        Caso base:
        map f . concat [] = map f (concat [])
                          = map f []
                          = []
                          = concat . map (map f) []

        pois
        concat . map (map f) [] = concat (map (map f) [])
                                = concat []
                                = []

        Não estou enxergando direito o problema. Nesta situação, a melhor coisa é testar uma situação simples, mas não ordinária:
        map f . concat [[a,b],[c,d]]
        map f (concat [[a,b],[c,d]])
        map f [a,b,c,d]
        [f a, f b, f c, f d]

        concat . map (map f) [[a,b],[c,d]]
        concat (map (map f) [[a,b],[c,d]])
        concat ([(map f [a,b]),(map f [c, d]))
        concat [[f a, f b], [f c, f d]]
        [f a, f b, f c, f d]

        Um pouco mais genérico:

        map f . concat [(a:as), (b:bs), ...]
        map f ((a:as) ++ (b:bs) ++ ...)
        map f (a:as) ++ map f (b:bs) ++ ...

        concat . map (map f) [(a:as), (b:bs), ...]
        concat [map f (a:as), map f (b:bs), ...]
        map f (a:as) ++ map f (b:bs) ++ ...

        Observo que não preciso mexer em (a:as), (b:bs) etc, de modo que poderia fazer
        map f . concat [as, bs, ...]
        map f (as ++ bs ++ ...)
        map f as ++ map f bs ++ ...

        concat . map (map f) [as, bs, ...]
        concat [map f as, map f bs, ...]
        map f as ++ map f bs ++ ...

        O que seria também similar a trocar [as, bs, ...] por (as:ass), onde as é uma lista e ass é uma lista de listas
        map f . concat (as:ass)
        map f (as ++ concat ass)
        map f as ++ map f (concat ass) <--- usei "map f as ++ map f bs = map f (as++bs)"  (A)
        map f as ++ map f . concat ass <-- Aqui usei a hipótese da indução
        map f as ++ concat . map (map f) ass 
        concat (map f as : map (map f) ass) <-- Usei "xs ++ concat xss = concat (xs:xss)" (B)
        concat (map (map f (as:ass))) <-- Usei "map f (x:xs) = (f x : map f x)" [definição de map]
        concat . map (map f) (as:ass)

        Agora preciso provar (A) e (B) acima, pois usei para chegar onde eu queria.
           
        Provando (A): do standart prelude,
        (++) :: [a] -> [a] -> [a]
        []     ++ ys = ys
        (x:xs) ++ ys = x : (xs ++ ys)
        
        Então,
        map f (a:as) ++ map f bss = (f a:map f as) ++ map f bss
                                  = f a : (map f as ++ map f bss)
                                  = f a : map f (as ++ bss) <-- Hipótese da indução
                                  = map f (a:as++bss)
                                  = map f ((a:as)++bss)

        E para o caso base:
        map f [] ++ map f bss = [] ++ map f bss
                              = map f bss
                              = map f ([]++bss)

        Provando (B): do standard prelude,
        concat :: [[a]] -> [a]
        concat xss = foldr (++) [] xss
        
        xs ++ concat xss = (x:xs') ++ concat xss
                         = x : xs' ++ concat xss
                         = x : concat (xs':xss)
                         = 
        -----

        f) Provar map f (xs ++ ys) = map f xs ++ map f ys
        Provei isso logo acima!

        -----
        
        g) Provar que, para f restrito, f . head = head . map f

        A parte da indução é fácil:
        head . map f (x:xs) = head (f x : map f xs)
                            = f (head (x:xs))
                            = f . head (x:xs)

        O problema é o caso base:
        f . head [] = f ?
        Aqui "?" representa o resultado "indefinido". Por outro lado,
        head . map f [] = head [] = ?
        Logo o resultado de f ? deve ser ? para que a igualdade seja válida. Ou seja,
        f ? = ?
        O que significa que f é restrita.
        
        -----

        h) Provar que foldr op e xs = foldl op e xs se
           i) op for associativa
           ii) e `op` x = x `op` e = x

        Passo base:
        foldr op e [] = e                            <-- Definição de foldr
                      = foldl op e []                <-- Definição de foldl

        Passo da indução:
        foldl op e (x:xs) = foldl op (e `op` x) xs   <-- Definição de foldl
                          = foldl op x xs            <-- e `op` x = x 
                          = foldr op x xs            <-- Hipótese da indução
                          = (foldr op x xs) `op` e   <-- x `op` e = x
                          = foldr op e (x:xs)        <-- NÃO TENHO CERTEZA DESSE PASSO!
                          

        Tentando pelo foldr...

        foldr op e (x:xs) = x `op` (foldr op e xs)                     <-- Definição de foldr
                          = x `op` (foldl op e xs)                     <-- Hipótese da indução
                          = x `op` (e `op` (x1 `op` (x2 `op` ... xN))) <-- Unfolding
                          = x `op` e `op` x1 `op` x2 `op` ... `op` xN  <-- `op` é associativo
                          = x `op` x1 `op` x2 `op` ... `op` xN         <-- x `op` e = x
                          = e `op` x `op` x1 `op` x2 `op` ... `op` xN  <-- e `op` x = x
                          = foldl op e (x:xs)                          <-- Folding

        -----
        
        i) 

        -----

        j) foldr op e xs = foldl (flip op) e (reverse xs)

        foldl (flip op) e (reverse (x:xs)) = foldl (flip op) e (reverse xs ++ [x])
                                           = foldl (flip op) ((flip op) e (head xs)) (tail xs ++ [x])
                                           = foldl (flip op) (op (head xs) e) (tail xs ++ [x])
                                           = foldl (flip op) (head xs) (tail xs ++ [x])
                                           = foldl (flip op) x' ((x':xs') ++ [x])
                                           = 



     -}

    -- Exercício 11.3 (p.141)
    -- ----------------------
    {-
        a) reverse ? = ?      Estrita
        b) simple a b c = a * (b + c)
           simple ? b c = ? * (b + c) = ? Estrita em a
           simple a ? c = a * (? + c) = ? Estrita em b
           simple a b ? = a * (b + ?) = ? Estrita em c
        c) map f ? = ?
           map f [?] = ? se f ? = ?
        d) tail ? = ?
        e) area ? = ?
        f) regionToGRegion ? = ?
        g) True && ? = ?
           False && ? = False
           ? && True = ?
           ? && False = ?
        h) (True &&) ? = ?
        i) (False &&) ? = False      NÃO estrita

        Embora (False &&) mostre uma situação claramenteo NÃO estrita, a impressão que tenho é que quase todas as funções são estritas. E faz sentido. Mas não sei se é assim mesmo ou se eu entendi errado.

     -}


    -- Exercício 11.4 (p.146)
    -- ----------------------
    (^!) :: Integer -> Integer -> Integer
    x^!n | n < 0     = error "negative exponent"
         | otherwise = ff x n
        where ff x n | n == 0    = 1
                     | even n    = fEven x n
                     | otherwise = x * fEven x (n-1)
              fEven x n = ff (x*x) (n `quot` 2)

    {-
        2^!23 = ff 2 23
              = 2 * fEven 2 22                              even 23 -> False
              = 2 * ff (2*2) (22 `quot` 2)
              = 2 * ff 4 11
              = 2 * 4 * fEven 4 10                          even 11 -> False
              = 2 * 4 * ff (4*4) (10 `quot` 2)
              = 2 * 4 * ff 16 5
              = 2 * 4 * 16 * fEven 16 4                     even 5 -> False
              = 2 * 4 * 16 * ff (16*16) (4 `quot` 2)
              = 2 * 4 * 16 * ff 256 2
              = 2 * 4 * 16 * fEven 256 2                    even 2 -> True
              = 2 * 4 * 16 * ff (256*256) * (2 `quot` 2)
              = 2 * 4 * 16 * ff 65536 1
              = 2 * 4 * 16 * 65536 * fEven 65536 0          even 1 -> False
              = 2 * 4 * 16 * 65536 * 1
              = 8 388 608

        2^!23 = ff 2 23
              = 2 * ff 2 22                                 even 23 -> False
              = 2 * ff (2*2) (22 `quot` 2)                  even 22 -> True
              = 2 * ff 4 11
              = 2 * 4 * ff 4 10                             even 11 -> False
              = 2 * 4 * ff (4*4) (10 `quot` 2)              even 10 -> True
              = 2 * 4 * ff 16 5
              = 2 * 4 * 16 * ff 16 4                        even 5 -> False
              = 2 * 4 * 16 * ff (16*16) (4 `quot` 2)        even 4 -> True
              = 2 * 4 * 16 * ff 256 2
              = 2 * 4 * 16 * ff (256*256) (2 `quot` 2)      even 2 -> True
              = 2 * 4 * 16 * ff 65536 1
              = 2 * 4 * 16 * 65536 * ff 65536 0             even 1 -> False
              = 2 * 4 * 16 * 65536 * 1
              = 8 388 608

        No primeiro caso [(^!) otimizado], foram necessárias 5 condicionais sobre a paridade de n; na segunda (não otimizada), 8.        
     -}

    
    -- Exercício 11.5
    -- --------------
    {-
        Passo base:
        fac1 0 = 1
        fac2 0 = fac' 0 1 = 1

        Passo da indução:
        fac1 (n+1) = (n+1) * fac1 n
                   = (n+1) * fac2 n <-- Hipótese da indução
                   = (n+1) * fac' n 1
                   = (n+1) * fac' (n-1) (n*1)
                   = (n+1) * fac' (n-2) (n * (n-1))
                   = (n+1) * fac' (n-3) (n * (n-1) * (n-2))
                   = (n+1) * fac' (n-n) (n * (n-1) * (n-2) * ... * 1)
                   = (n+1) * n * (n-1) * (n-2) * ... * 1
                   = fac2 (n+1)   cf. abaixo

        fac2 (n+1) = fac' (n+1) 1
                   = fac' (n) (n+1)
                   = fac' (n-1) ((n+1) * n)
                   = fac' (n-2) ((n+1) * n * (n-1))
                   = ...
                   = fac' (n-n) ((n+1) * n * (n-1) * ... * 1)
                   = (n+1) * n * (n-1) * ... * 1
     -}

