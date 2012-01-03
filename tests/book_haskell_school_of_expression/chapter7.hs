module Chapter7 where

    data Tree a = Leaf a | Branch (Tree a) (Tree a) deriving Show

    -- -------------
    -- Exercício 7.1
    -- -------------

    fringe :: Tree a -> [a]
    fringe (Leaf x) = [x]
    fringe (Branch t1 t2) = fringe t1 ++ fringe t2

    treeSize :: Tree a -> Integer
    treeSize (Leaf x) = 1
    treeSize (Branch t1 t2) = treeSize t1 + treeSize t2

    myTree :: Tree Int
    myTree = Branch
             (
               Leaf 10
             )
             (
               Branch
                 (Leaf 2)
                 (Leaf 30)
             )

    -- Encontrei esta solução na Internet
    foldTree :: (a -> b -> b) -> (b -> b -> b) -> b -> Tree a -> b
    foldTree foldLeaf foldBranch init (Leaf x) = foldLeaf x init
    foldTree foldLeaf foldBranch init (Branch x y) = foldBranch x' y'
        where x' = foldTree foldLeaf foldBranch init x
              y' = foldTree foldLeaf foldBranch init y


    higherOrderFringe :: Tree a -> [a]
    higherOrderFringe tree = foldTree (:) (++) [] tree
    {-
      Entendendo...
      (:) é uma função que recebe a e [a] e retorna [a]. Ou seja:
      (:) :: a -> [a] -> [a]
      (:) acrescenta um elemento no começo de uma lista.

      Vou usar myTree (acima) para entender o funcionamento do higherOrderFringe

      higherOrderFringe (:) (++) [] (Branch(Leaf 10)(Branch(Leaf 2)(Leaf 30)))
                                            ---x---  ------------y----------
      (++) x' y'
        => x' = foldTree (:) (++) [] (Leaf 10)
              = (:) 10 []
              = [10]
        => y' = foldTree (:) (++) [] (Branch(Leaf 2)(Leaf 30)))
                                            -- x --  -- y --
              = (++) x' y'
                => x' = foldTree (:) (++) [] (Leaf 2)
                      = (:) 2 []
                      = [2]
                => y' = foldTree (:) (++) [] (Leaf 30)
                      = (:) 30 []
                      = [30]
              = (++) [2] [30]
              = [2,30]
      (++) [10] [2,30]
      [10,2,30]

      O meu pecado neste exercício foi não ter enxergado o que fazer com a (Leaf x). Logo antes
      de procurar a resposta na Internet, eu vislumbrei a necessidade de uma segunda função (de
      fold, depois eu descobri que este é o nome), mas não tinha a menor ideia de como ela deveria
      ser. Também estou bastante perdido na definição das assinaturas das funções!

    -}

    -- Ok, esta função eu consegui definir sozinho
    higherOrderTreeSize :: Tree a -> Int
    higherOrderTreeSize tree = foldTree (\_ x -> x + 1) (+) 0 tree

    -- Não entendi por que o max funciona! Entendi o porquê, mas não o como.
    higherOrderTreeHeight :: Tree a -> Int
    higherOrderTreeHeight tree = foldTree (\_ _ -> 0) (\x y -> 1 + max x y) 0 tree

    {-
       Acima, na função (\x y -> 1 + max x y), os argumentos x e y são, de fato,
       a "altura"/"profundidade" dos dois Branch's a que esta associada a função.
       Entender o "max x y", então, é simples. O que é difícil de ENXERGAR é que de fato
       x e y são a "altura"/"profundidade". Embora a simulação abaixo torne isto evidente,
       não é imediata essa conclusão. Realmente requer bastante raciocínio!...

       Entendendo...
       Vou chamar fLeaf x y = (\_ _ -> 0) = 0 e
                  fBranch x y = (\x y -> 1 + max x y)

       higherOrderTreeHeight (Branch(Leaf 10)(Branch(Leaf 2)(Leaf 30)))
       foldTree fLeaf fBranch 0 (Branch(Leaf 10)(Branch(Leaf 2)(Leaf 30)))
                                        -- x --  ---------- y -----------
       fBranch x' y'
            where x' = foldTree fLeaf fBranch 0 (Leaf 10)
                     = fLeaf 10 0
                     = 0
                  y' = foldTree fLeaf fBranch 0 (Branch(Leaf 2)(Leaf 30)))
                                                        - x --  -- y --
                     = fBranch x'' y''
                        where x'' = foldTree fLeaf fBranch 0 (Leaf 2)
                                  = fLeaf 2 0
                                  = 0
                              y'' = foldTree fLeaf fBranch 0 (Leaf 30)
                                  = fLeaf 30 0
                                  = 0
                     = fBranch 0 0
                     = 1 + max 0 0
                     = 1
       fBranch 0 1
       1 + max 0 1
       2
    -}

	
    data InternalTree a = ILeaf
                        | IBranch a (InternalTree a) (InternalTree a)
                          deriving Show
 
    takeTree :: Int -> InternalTree a -> InternalTree a
    takeTree 0 t = ILeaf
    takeTree n ILeaf = ILeaf
    takeTree n (IBranch a x y) = IBranch a (takeTree (n-1) x) (takeTree (n-1) y)
 
    takeTreeWhile :: (a -> Bool) -> InternalTree a -> InternalTree a
    takeTreeWhile f ILeaf = ILeaf
    takeTreeWhile f (IBranch a x y) = if (f a)
                                      then IBranch a (takeTreeWhile f x)
                                                     (takeTreeWhile f y)
                                      else ILeaf

    mytree :: InternalTree Int
    mytree = IBranch 1
             ( 
                IBranch 2
                    (
                    ILeaf
                    )(
                        IBranch 3
                        (
                            ILeaf
                        )(
                            IBranch 4 
                            (
                                ILeaf
                            )(
                                ILeaf
                            )
                        )
                    )
             )(
                ILeaf
             )

