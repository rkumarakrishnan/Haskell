module Chapter12 where

    -- Exercício 12.1 (p.161)
    -- ----------------------
    {-
        Definição de Tree na classe Eq:
        instance Eq a => Eq (Tree a) where
            Leaf a == Leaf b                = a == b
            Branch l1 r1 == Branch l2 r2    = l1 == l2 && r1 == r2
            _ == _                          = False

        As leis da classe Eq (standard prelude):
        class Eq a where
            (==), (/=) :: a -> a -> Bool
            x /= y      = not (x == y)
            x == y      = not (x /= y)

        Ou seja, eu preciso demonstrar que uma Tree qualquer obedece essas leis, pois Tree pertence a Eq

        Primeiro, testando t1 == t2
        Caso 1: t1 = Leaf a, t2 = Leaf b

        t1 == t2
        Leaf a == Leaf b
        a == b. Como a e b são Eq, o operador == funciona neles, e este caso fica provado.

        Caso 2: t1 = Branch t11 t12, t2 = Leaf c
        
        t1 == t2
        Branch t11 t22 == Leaf c
        False. Chegou a um resultado Bool, que é o objetivo.

        Caso 3: t1 = Leaf a, t2 = Branch t21 t22
        
        t1 == t2
        Leaf a == Branch t21 t22
        False. Idem ao caso 2.

        Caso 4: t1 = Branch t11 t22 e t2 = Branch t21 t22

        t1 == t2
        Branch t11 t22 == Branch t21 t22
        t11 == t22 && t21 == t22, que por sua vez recaem em um desses quatro casos e, em última instância, num dos 3 casos acima, cuja avaliação é garantida: nos casos 2 e 3 é simplesmente False; no caso 1 depende do valor nas folhas, que são Ord e, portanto, tem avaliação garantida.

        A prova de /= é a mesma, exceto que aparece um not antes de cada caso:
        Caso 1: not (a == b)
        Caso 2: not False = True
        Caso 3: not False = True
        Caso 4: not (t11 == t21 && t21 == t22)

        A lógica para demonstrar que Tree pertence a Ord é a mesma... não vou fazer isso.
     -}

    -- Exercício 12.3
    -- --------------
    quicksort :: Ord a => [a] -> [a]
    quicksort [] = []
    quicksort (x:xs) = smaller ++ [x] ++ greater
        where smaller = quicksort (filter (<x) xs)
              greater = quicksort (filter (>=x) xs)
