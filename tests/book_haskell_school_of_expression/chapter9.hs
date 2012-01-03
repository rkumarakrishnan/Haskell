module Chapter9 where

    -- Exercício 9.1 (p.108)
    -- ---------------------
    {-
	Veja a definição de (Polygon pts) `containsR` p no arquivo Region.hs
     -}

    -- Exercício 9.2
    -- -------------
    {-
	flip (flip f) x y = (flip f) y x = flip f y x = f x y
	Portanto (fazendo currying duas vezes, em x e y), flip (flip f) = f
     -}

    -- Exercício 9.3
    -- -------------
    {-
	xs = [1,2,3] :: [Float]
	ys = map (+) xs

	map :: (a -> b) -> [a] -> [b], com a = Float neste caso
	(+) :: a -> a -> a = a -> (a -> a) = a -> b, com b = (a -> a)

	map :: (a -> b) -> [a] -> [b]
	map (+) :: [a] -> [b]
	map (+) xs :: [b]
		   :: [a -> a]
		   :: [Float -> Float]   
     -}

    -- Exercício 9.4
    -- -------------

    applyEach :: [a -> b] -> a -> [b]
    applyEach [] _ = [] 
    applyEach (f:fs) x = f x : applyEach fs x

    -- Exercício 9.5
    -- -------------

    applyAll :: [a -> a] -> a -> a -- Como a aplicação da função é em cascata, a saída de uma função deve ser do mesmo tipo de entrada da outra...
--    applyAll (f:fs) x = f (applyAll fs x) -- Versão com recursividade (esta versão NÃO resolve, sozinha, o caso da lista de funções vazia)
--    applyAll (f:fs) x = foldl (.) f fs x -- Versão com função de ordem superior (esta versão NÃO resolve, sozinha, o caso da lista de funções vazia)
--    applyAll fs x = foldl (.) (\y -> y) fs x -- Versão 2 com função de ordem superior (esta versão dá conta do caso em que a lista de funções é vazia)
    applyAll fs x = foldl (.) id fs x -- Descobri que "id" é a função identidade, igual àquela que defini na linha acima.
    
    -- Exercício 9.6
    -- -------------
    {-
	appendl [a,b,c]   <-- a, b, c são listas.
	  = foldl (flip (++)) [] [a,b,c]
	  = flip (++) (flip (++) (flip (++) [] a) b) c
	  = flip (++) (flip (++) ((++) a []) b) c
	  = flip (++) ((++) b ((++) a [])) c
	  = (++) c ((++) b ((++) a []))  
	  = (++) c ((++) b (a ++ [])) <-- n_a passos 
	  = (++) c (b ++ a ++ []) <-- n_b passos
	  = c ++ b ++ a ++ [] <-- n_c passos
	  = [c,b,a]
    Ou seja, a quantidade total de passos � n_a+n_b+n_c. Ou seja, O(n)

	appendr [a,b,c]
	  = foldr (flip (++)) [] [a,b,c]
	  = flip (++) a (flip (++) b (flip (++) c []))
	  = flip (++) a (flip (++) b ((++) [] c))
	  = flip (++) a ((++) ((++) [] c) b)
	  = (++) ((++) ((++) [] c) b) a
	  = (++) ((++) ([] ++ c) b) a  <-- 1 passo
	  = (++) ([] ++ c ++ b) a <-- n_c passos
	  = [] ++ c ++ b ++ a <-- n_c+n_b passos
	  = [c,b,a]
    Se houvesse mais listas a concatenar, a quantidade total de passos seria
    n_1 + (n_1+n_2) + (n_1+n_2+n_3) + ... n parcelas --> O(n^2)

    Portanto, appendr � O(n^2) enquanto appendl � O(n). Logo, appendl � melhor.


    O resumo � simples: devido � forma recursiva como a lista � definida, toda vez que se faz um append
    � preciso passar por todos os n elementos da lista. Se houver k listas com comprimento n_1, n_2, ... n_k,
    ent�o a quantidade de passos para fazer a concatena��o � direita (append), com foldr, por exemplo, seria
    1 + n_1 + (n_1+n_2) + (n_1+n_2+n_3) + ... + (n_1+n_2+...+n_k)  <-- k parcelas (inclusive o 1)
     = 1 + (n_k-1)*n_1 + (n_k-2)*n_2 + (n_k-3)*n_3 + ... + n_k
     = 1 + (n-1)*n + (n-2)*n + (n-3)*n + ... + n <-- Fazendo n_1 = n_2 = n_3 ... para simplificar
     = 1 + n * (n-1 + n-2 + n-3 + ... + 1)
     = 1 + n * (n-1+1) * n / 2

    1                         <-- Concatena a lista 1 com []
    + n_1                     <-- Concatena a lista 2 com l_1
    + n_1+n_2                 <-- Concatena a lista 3 com l_1++l_2
    + n_1+n_2+n_3             <-- Concatena a lista 4 com l_1++l_2++l_3
    + ...
    + n_1+n_2+n_3+...+n_(k-1) <-- Concatena a lista k com l_1++l_2++l_3++...++l_(k-1)
    = 1 + n_1*(k-1) + n_2*(k-2) + ... n_(k-1)*1
    = 1 + n*(k-1) + n*(k-2) + ... n*1
    = 1 + n*(k-1 + k-2 + ... + 1)
    = 1 + n*(k-1+1)*k/2
    = 1 + n*k^2/2


     -}

    appendl, appendr :: [[a]] -> [a]
    appendl = foldl (flip (++)) []
    appendr = foldr (flip (++)) []

    -- Exerc�cio 9.7 (p.109)
    -- ---------------------

--    twice :: (a -> a) -> a -> a
--    twice op x = op $ op x -- � a mesma coisa que "op (op x)". O $ torna os par�nteses desnecess�rios.

    twice :: (a -> a) -> (a -> a)
    twice op = op . op

    {-
        twice twice f
          = twice . twice f
          = twice (f . f)
          = (f . f) . (f . f)
          = f . f . f . f

        Exemplo:
        twice twice (+1) 2 =
          = ((((2+1)+1)+1)+1)
          = 6

        twice twice twice f
          = (twice twice f) . (twice twice f)
          = twice twice (f.f)
          = twice ((f.f).(f.f))
          = ((f.f).(f.f)).((f.f).(f.f))
          = f.f.f.f.f.f.f.f <--- Devia ser 16 aplica��es de f, mas n�o sei como chegar nisso!
     -} 

    -- Exerc�cio 9.8
    -- -------------
--    power :: (a -> a) -> Int -> a -> a
--    power op 1 x = op x
--    power op n x = power op (n-1) (op x)

    power :: (a -> a) -> Int -> a -> a
    power op n x = foldl (.) id (replicate n op) x

    -- Qual das duas vers�es acima � melhor?

    potencia :: Float -> Int -> Float
    potencia x n = power (*x) n 1

    -- Exerc�cio 9.9 (p.111)
    -- ---------------------
    {-
        fix f = f (fix f)                                          (0)
   
        f :: a -> b   <-- Isto vale sempre (totalmente arbitr�rio) (1)
        fix :: c -> d  <-- Isto tamb�m vale sempre                 (2)

        Ao colocar f como argumento de fix em "fix f", estamos fazendo f :: c (2) = a -> b (1). Assim,
        fix f :: d

        Agora, observamos em (0) que "fix f" � argumento de f, e portanto
        fix f :: a
        Logo, a = d

        Olhando para (1), f retorna b, que deve ser igual ao tipo de "fix f", que � a = d.
        Portanto, b = a = d

        Resumindo:
        f :: a -> a
        fix :: (a -> a) -> a

        Testando:
        fix :: (a -> a) -> a
        fix f :: a                                                  (3)

        f :: a -> a
        f (fix f) :: a                                              (4)
        
        Comparando (3) e (4) podemos concluir que est� coerente.




        10 / 2

        op :: Int -> Int -> Int
        op b a = if (a < b) then a else (a-b)
    
        op 2 :: Int -> Int
        op 2 a = if (a < 2) then a else (a-2)
    
        (op 2) � a fun��o f que uso em fix.

        fix (op 2) = op 2 (fix (op 2))
                   = op 2 (op 2 (fix (op 2)))

     -}

    fix :: (a -> a) -> a
    fix f = f (fix f)

    remainder = fix (\f a b -> if a < b then a else f (a-b) b) -- N�O ENTENDI!! N�O FOI PRECISO NEM DEFINIR f!!!!!!!!!!


    -- Exerc�cio 9.10 (p.113)
    -- ----------------------
    {-
        map (\x -> (x+1)/2) xs

        Para cada elemento de xs, deve-se:
        1) somar 1
        2) dividir por 2
        Ou seja, (x+1)/2 = (/2).(+1) x

        Portanto podemos reescrever a instru��o acima desta forma:
        map ((/2).(+1)) xs
     -}

    -- Exerc�cio 9.11
    -- --------------
    {-
        map f (map g xs)
          = map (f.g) xs

        Deste modo, a instru��o do exerc�cio anterior tamb�m pode ser escrita assim:
        map (/2) (map (+1) xs)
     -}

    -- Exerc�cio 9.12
    -- --------------
    -- T� louco!... N�o vou fazer isso n�o.
    
